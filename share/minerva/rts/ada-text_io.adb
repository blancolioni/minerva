with Basic_IO;

package body Ada.Text_IO is

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Basic_IO.Put_Ascii (10);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Ch : Character) is
   begin
      Basic_IO.Put_Char (Ch);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Ch : Character) is
   begin
      Put (Ch);
      New_Line;
   end Put_Line;

end Ada.Text_IO;
