package body Ada.Text_IO is

   Tty_Enable : Byte with Address => 16#FF80#;
   Tty_Char_Buffer : Character with Address => 16#FF81#;
   Tty_Byte_Buffer : Byte with Address => 16#FF81#;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Tty_Byte_Buffer := 10;
      Tty_Enable := 1;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Ch : Character) is
   begin
      Tty_Char_Buffer := Ch;
      Tty_Enable := 1;
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
