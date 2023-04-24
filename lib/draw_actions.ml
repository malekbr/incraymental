open! Core

module Rectangle = struct
  type t =
    { x : float
    ; y : float
    ; w : float
    ; h : float
    }

  let raylib { x; y; w; h } = Raylib.Rectangle.create x y w h
  let scale t s = { t with w = t.w *. s; h = t.h *. s }
end

module Vector2 = struct
  type t =
    { x : float
    ; y : float
    }
  [@@deriving sexp, equal, compare]

  module O = struct
    let ( + ) a b = { x = a.x +. b.x; y = a.y +. b.y }
    let ( - ) a b = { x = a.x -. b.x; y = a.y -. b.y }
    let ( * ) a s = { x = a.x *. s; y = a.y *. s }
  end

  include O

  let create x y = { x; y }
  let raylib { x; y } = Raylib.Vector2.create x y
end

module Camera2D = struct
  type t =
    { offset : Vector2.t
    ; target : Vector2.t
    ; rotation : float
    ; zoom : float
    }

  let center
    ?(rotation = 0.)
    ?(zoom = 1.)
    (rectangle : Rectangle.t)
    ~canvas_width
    ~canvas_height
    =
    { offset =
        { x = (Float.of_int canvas_width -. rectangle.w) /. 2.
        ; y = (Float.of_int canvas_height -. rectangle.h) /. 2.
        }
    ; target = { x = rectangle.x; y = rectangle.y }
    ; rotation
    ; zoom
    }
  ;;

  let raylib { offset; target; rotation; zoom } =
    Raylib.Camera2D.create (Vector2.raylib offset) (Vector2.raylib target) rotation zoom
  ;;
end

module Color : sig
  include module type of Raylib.Color
end = struct
  include Raylib.Color
end

module Draw_style = struct
  type t =
    | Lines of { thickness : float option }
    | Fill
end

module Loadable_texture : sig
  type t

  val create : Raylib.Image.t -> t
  val image : t -> Raylib.Image.t
  val width : t -> int
  val height : t -> int
  val rect : ?top_left:Vector2.t -> t -> Rectangle.t
  val get_or_load : t -> Raylib.Texture2D.t
  val unload : t -> unit
  val diff : t list -> not_in:t list -> t list
end = struct
  module Id = Unique_id.Int63 ()

  type t =
    { source : Raylib.Image.t
    ; id : Id.t
    ; mutable texture : Raylib.Texture2D.t option
    }

  let image t = t.source
  let width t = Raylib.Image.width t.source
  let height t = Raylib.Image.height t.source
  let create source = { source; id = Id.create (); texture = None }

  let rect ?(top_left = { Vector2.x = 0.; y = 0. }) t =
    let { Vector2.x; y } = top_left in
    { Rectangle.x
    ; y
    ; w = Raylib.Image.width t.source |> Float.of_int
    ; h = Raylib.Image.height t.source |> Float.of_int
    }
  ;;

  let get_or_load t =
    match t.texture with
    | None ->
      let texture = Raylib.load_texture_from_image t.source in
      t.texture <- Some texture;
      texture
    | Some texture -> texture
  ;;

  let unload t =
    Option.iter ~f:Raylib.unload_texture t.texture;
    t.texture <- None
  ;;

  let diff ts ~not_in =
    let ids = List.map not_in ~f:(fun t -> t.id) |> Id.Set.of_list in
    List.filter ts ~f:(fun t -> not (Set.mem ids t.id))
  ;;
end

module Instructions = struct
  type 'a t =
    | Rectangle :
        { rect : Rectangle.t
        ; style : Draw_style.t
        ; color : Color.t
        }
        -> [> `Primitive ] t
    | Text :
        { content : string
        ; position : Vector2.t
        ; font_size : int
        ; color : Color.t
        }
        -> [> `Primitive ] t
    | Line :
        { start : Vector2.t
        ; stop : Vector2.t
        ; thickness : float option
        ; color : Color.t
        }
        -> [> `Primitive ] t
    | Texture :
        { texture : Loadable_texture.t
        ; source : Rectangle.t
        ; target : Rectangle.t
        ; tint : Color.t
        }
        -> [> `Primitive ] t
    | Many : 'a t list -> 'a t
    | Mode_2d : Camera2D.t * [ `Primitive ] t -> [> `Primitive | `Camera2D ] t

  let rec perform : type a. a t -> unit = function
    | Rectangle { rect; style; color } ->
      (match style with
       | Lines { thickness = None } ->
         Raylib.draw_rectangle_lines
           (Int.of_float rect.x)
           (Int.of_float rect.y)
           (Int.of_float rect.w)
           (Int.of_float rect.h)
           color
       | Lines { thickness = Some thickness } ->
         Raylib.draw_rectangle_lines_ex (Rectangle.raylib rect) thickness color
       | Fill -> Raylib.draw_rectangle_rec (Rectangle.raylib rect) color)
    | Line { start; stop; thickness; color } ->
      (match thickness with
       | None ->
         Raylib.draw_line
           (Int.of_float start.x)
           (Int.of_float start.y)
           (Int.of_float stop.x)
           (Int.of_float stop.y)
           color
       | Some thickness ->
         Raylib.draw_line_ex (Vector2.raylib start) (Vector2.raylib stop) thickness color)
    | Text { content; position; font_size; color } ->
      Raylib.draw_text
        content
        (Int.of_float position.x)
        (Int.of_float position.y)
        font_size
        color
    | Texture { texture; source; target; tint } ->
      Raylib.draw_texture_pro
        (Loadable_texture.get_or_load texture)
        (Rectangle.raylib source)
        (Rectangle.raylib target)
        (Raylib.Vector2.zero ())
        0.
        tint
    | Many ts -> List.iter ~f:perform ts
    | Mode_2d (camera, primitive) ->
      Raylib.begin_mode_2d (Camera2D.raylib camera);
      perform primitive;
      Raylib.end_mode_2d ()
  ;;

  let simple_texture ?(tint = Color.white) texture ~top_left =
    Texture
      { texture
      ; tint
      ; source = Loadable_texture.rect texture
      ; target = Loadable_texture.rect ~top_left texture
      }
  ;;

  module Packed = struct
    type 'a outer = 'a t
    type t = T : 'a outer -> t
  end

  let rec fold_preorder (Packed.T t as packed) ~init ~f =
    match t with
    | Many ts ->
      let init = f init packed in
      List.fold ts ~init ~f:(fun init t -> fold_preorder (T t) ~init ~f)
    | Mode_2d (_, primitive) ->
      let init = f init packed in
      fold_preorder (T primitive) ~init ~f
    | Texture _ | Text _ | Line _ | Rectangle _ -> f init packed
  ;;
end

type t =
  { instructions : Instructions.Packed.t
  ; background_color : Color.t
  }

let empty = { instructions = T (Many []); background_color = Color.white }

let perform { instructions = T instructions; background_color } =
  Raylib.begin_drawing ();
  Raylib.clear_background background_color;
  Instructions.perform instructions;
  Raylib.end_drawing ()
;;

let unload t ~old =
  let get_textures t =
    Instructions.fold_preorder t.instructions ~init:[] ~f:(fun acc (T t) ->
      match t with
      | Texture { texture; _ } -> texture :: acc
      | _ -> acc)
  in
  Loadable_texture.diff ~not_in:(get_textures t) (get_textures old)
  |> List.iter ~f:Loadable_texture.unload
;;
