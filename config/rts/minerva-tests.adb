with Ada.Text_IO;

package body Minerva.Tests is

   procedure Result (Value : Boolean) is
   begin
      if Value then
         Ada.Text_IO.Put ('y');
         Ada.Text_IO.Put ('e');
         Ada.Text_IO.Put ('s');
      else
         Ada.Text_IO.Put ('n');
         Ada.Text_IO.Put ('o');
      end if;

      Ada.Text_IO.New_Line;
   end Result;

end Minerva.Tests;
