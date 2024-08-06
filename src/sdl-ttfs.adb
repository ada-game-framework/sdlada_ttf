--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.TTFs
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C.Strings;
with SDL.Error;

package body SDL.TTFs is
   package Palettes renames SDL.Video.Palettes;

   function Initialise return Boolean is
      function TTF_Init return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_Init";
   begin
      return (TTF_Init = Success);
   end Initialise;


   overriding
   procedure Finalize (Self : in out Fonts) is
      procedure TTF_Close_Font (Font : in Fonts_Ref) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_CloseFont";
   begin
      if Self.Internal /= null then
         if Self.Source_Freed = False then
            TTF_Close_Font (Self.Internal);
         end if;

         Self.Internal := null;
      end if;
   end Finalize;


   function Style (Self : in Fonts) return Font_Styles is
      function TTF_Get_Font_Style (Font : in Fonts_Ref) return Font_Styles with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontStyle";
   begin
      return TTF_Get_Font_Style (Self.Internal);
   end Style;


   procedure Set_Style (Self : in out Fonts; Now : in Font_Styles) is
      procedure TTF_Set_Font_Style (Font : in Fonts_Ref; Now : in Font_Styles) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontStyle";
   begin
      TTF_Set_Font_Style (Self.Internal, Now);
   end Set_Style;


   function Outline (Self : in Fonts) return Font_Outlines is
      function TTF_Get_Font_Outline (Font : in Fonts_Ref) return Font_Outlines with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontOutline";
   begin
      return TTF_Get_Font_Outline (Self.Internal);
   end Outline;


   procedure Set_Outline (Self : in out Fonts; Now : in Font_Outlines := Outlines_Off) is
      procedure TTF_Set_Font_Outline (Font : in Fonts_Ref; Now : in Font_Outlines) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontOutline";
   begin
      TTF_Set_Font_Outline (Self.Internal, Now);
   end Set_Outline;


   function Hinting (Self : in Fonts) return Font_Hints is
      function TTF_Get_Font_Hinting (Font : in Fonts_Ref) return Font_Hints with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontHinting";
   begin
      return TTF_Get_Font_Hinting (Self.Internal);
   end Hinting;


   procedure Set_Hinting (Self : in out Fonts; Now : in Font_Hints := Normal) is
      procedure TTF_Set_Font_Hinting (Font : in Fonts_Ref; Now : in Font_Hints) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontHinting";
   begin
      TTF_Set_Font_Hinting (Self.Internal, Now);
   end Set_Hinting;


   function Kerning (Self : in Fonts) return Boolean is
      function TTF_Get_Font_Kerning (Font : in Fonts_Ref) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontKerning";
   begin
      return (TTF_Get_Font_Kerning (Self.Internal) = Success);
   end Kerning;


   procedure Set_Kerning (Self : in out Fonts; Now : in Boolean) is
      procedure TTF_Set_Font_Kerning (Font : in Fonts_Ref; Now : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontKerning";
   begin
      TTF_Set_Font_Kerning (Font => Self.Internal,
                            Now  => (if Now then 1 else 0));
   end Set_Kerning;


   function Height (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Height (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontHeight";
   begin
      return TTF_Font_Height (Self.Internal);
   end Height;


   function Ascent (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Ascent (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontAscent";
   begin
      return TTF_Font_Ascent (Self.Internal);
   end Ascent;


   function Descent (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Descent (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontDescent";
   begin
      return TTF_Font_Descent (Self.Internal);
   end Descent;


   function Line_Skip (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Line_Skip (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontLineSkip";
   begin
      return TTF_Font_Line_Skip (Self.Internal);
   end Line_Skip;


   function Faces (Self : in Fonts) return Font_Faces is
      function TTF_Font_Faces (Font : in Fonts_Ref) return Font_Faces with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaces";
   begin
      return TTF_Font_Faces (Self.Internal);
   end Faces;


   function Is_Face_Fixed_Width (Self : in Fonts) return Boolean is
      function TTF_Font_Face_Is_Fixed_Width (Font : in Fonts_Ref) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaceIsFixedWidth";
   begin
      return (TTF_Font_Face_Is_Fixed_Width (Self.Internal) > Success);
   end Is_Face_Fixed_Width;


   function Face_Family_Name (Self : in Fonts) return String is
      function TTF_Font_Face_Family_Name (Font : in Fonts_Ref) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaceFamilyName";
   begin
      return C.Strings.Value (TTF_Font_Face_Family_Name (Self.Internal));
   end Face_Family_Name;


   function Face_Style_Name (Self : in Fonts) return String is
      function TTF_Font_Face_Style_Name (Font : in Fonts_Ref) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaceStyleName";
   begin
      return C.Strings.Value (TTF_Font_Face_Style_Name (Self.Internal));
   end Face_Style_Name;


   function Size_Latin_1 (Self : in Fonts; Text : in String) return SDL.Sizes is
      function TTF_Size_Text (Font : in Fonts_Ref;
                              Text : in C.char_array;
                              W    : out Dimension;
                              H    : out Dimension) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SizeText";

      Size : SDL.Sizes := SDL.Zero_Size;
   begin
      if TTF_Size_Text (Self.Internal, C.To_C (Text), Size.Width, Size.Height) /= Success then
         raise TTF_Error with SDL.Error.Get;
      end if;

      return Size;
   end Size_Latin_1;


   function Size_UTF_8 (Self : in Fonts; Text : in UTF_Strings.UTF_8_String) return SDL.Sizes is
      function TTF_Size_UTF_8 (Font : in Fonts_Ref;
                               Text : in C.char_array;
                               W    : out Dimension;
                               H    : out Dimension) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SizeUTF8";

      Size : SDL.Sizes := SDL.Zero_Size;
   begin
      if TTF_Size_UTF_8 (Self.Internal, C.To_C (Text), Size.Width, Size.Height) /= Success then
         raise TTF_Error with SDL.Error.Get;
      end if;

      return Size;
   end Size_UTF_8;


   function Make_Surface_From_Pointer (S    : in Video.Surfaces.Internal_Surface_Pointer;
                                       Owns : in Boolean := False) return Video.Surfaces.Surface with
     Import     => True,
     Convention => Ada;


   --  This is to get around C programmers being inconsistent. We may need more of these.
   --  The functions in SDL2_TTF use `SDL_Color fg` whereas the rest of SDL2 uses `const SDL_Color *`,
   --  The Colour record is convention C and will be passed correctly with an in parameter to C.
   type Colour_By_Copy is new Colour with
     Convention => C_Pass_by_Copy;


   function Render_Solid (Self   : in Fonts;
                          Text   : in String;
                          Colour : in Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_Text_Solid (Font   : in Fonts_Ref;
                                      Text   : in C.char_array;
                                      Colour : in Palettes.Colour_By_Copy)
                                      return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderText_Solid";

   begin
      return Make_Surface_From_Pointer
           (S    => TTF_Render_Text_Solid (Self.Internal,
                                           C.To_C (Text),
                                           Palettes.Colour_By_Copy (Colour)),
            Owns => True);
   end Render_Solid;


   function Render_Shaded (Self              : in Fonts;
                           Text              : in String;
                           Colour            : in Palettes.Colour;
                           Background_Colour : in Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_Text_Shaded (Font              : in Fonts_Ref;
                                       Text              : in C.char_array;
                                       Colour            : in Palettes.Colour_By_Copy;
                                       Background_Colour : in Palettes.Colour_By_Copy)
                                       return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderText_Shaded";
   begin
      return Make_Surface_From_Pointer
           (S    => TTF_Render_Text_Shaded (Self.Internal,
                                            C.To_C (Text),
                                            Palettes.Colour_By_Copy (Colour),
                                            Palettes.Colour_By_Copy (Background_Colour)),
            Owns => True);
   end Render_Shaded;


   function Render_Blended (Self   : in Fonts;
                            Text   : in String;
                            Colour : in Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_Text_Blended (Font   : in Fonts_Ref;
                                        Text   : in C.char_array;
                                        Colour : in Palettes.Colour_By_Copy)
                                        return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderText_Blended";
   begin
      return Make_Surface_From_Pointer
           (S    => TTF_Render_Text_Blended (Self.Internal,
                                             C.To_C (Text),
                                             Palettes.Colour_By_Copy (Colour)),
            Owns => True);
   end Render_Blended;


   function Render_UTF_8_Solid (Self     : in Fonts;
                                Text     : in UTF_Strings.UTF_8_String;
                                Colour   : in Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_UTF_8_Solid (Font    : in Fonts_Ref;
                                       Text    : in C.char_array;
                                       Colour  : in Palettes.Colour_By_Copy)
                                       return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderUTF8_Solid";
   begin
      return Make_Surface_From_Pointer
           (S    => TTF_Render_UTF_8_Solid (Self.Internal,
                                            C.To_C (Text),
                                            Palettes.Colour_By_Copy (Colour)),
            Owns => True);
   end Render_UTF_8_Solid;


   function Render_UTF_8_Shaded (Self              : in Fonts;
                                 Text              : in UTF_Strings.UTF_8_String;
                                 Colour            : in Palettes.Colour;
                                 Background_Colour : in Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_UTF_8_Shaded (Font              : in Fonts_Ref;
                                        Text              : in C.char_array;
                                        Colour            : in Palettes.Colour_By_Copy;
                                        Background_Colour : in Palettes.Colour_By_Copy)
                                        return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderUTF8_Shaded";
   begin
      return Make_Surface_From_Pointer
           (S    => TTF_Render_UTF_8_Shaded (Self.Internal,
                                             C.To_C (Text),
                                             Palettes.Colour_By_Copy (Colour),
                                             Palettes.Colour_By_Copy (Background_Colour)),
            Owns => True);
   end Render_UTF_8_Shaded;


   function Render_UTF_8_Blended (Self              : in Fonts;
                                  Text              : in UTF_Strings.UTF_8_String;
                                  Colour            : in Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_UTF_8_Blended (Font   : in Fonts_Ref;
                                         Text   : in C.char_array;
                                         Colour : in Palettes.Colour_By_Copy)
                                         return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderUTF8_Blended";
   begin
      return Make_Surface_From_Pointer
           (S    => TTF_Render_UTF_8_Blended (Self.Internal,
                                              C.To_C (Text),
                                              Palettes.Colour_By_Copy (Colour)),
            Owns => True);
   end Render_UTF_8_Blended;
end SDL.TTFs;
