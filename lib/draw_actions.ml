open! Core

module Rectangle = struct
  type t =
    { x : float
    ; y : float
    ; w : float
    ; h : float
    }

  let raylib { x; y; w; h } = Raylib.Rectangle.create x y w h
end

module Vector2 = struct
  type t =
    { x : float
    ; y : float
    }
  [@@deriving sexp_of, compare]

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
        { texture : Raylib.Texture2D.t
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
        texture
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

  module Packed = struct
    type 'a outer = 'a t
    type t = T : 'a outer -> t
  end
end

type t =
  { instructions : Instructions.Packed.t
  ; background_color : Color.t
  }

let perform { instructions = T instructions; background_color } =
  Raylib.begin_drawing ();
  Raylib.clear_background background_color;
  Instructions.perform instructions;
  Raylib.end_drawing ()
;;
