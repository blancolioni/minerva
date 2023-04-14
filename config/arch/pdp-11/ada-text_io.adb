package body Ada.Text_IO is

   TTY_TPS      : Byte with Address => 16#FF74#;
   TTY_TPB_Char : Character with Address => 16#FF76#;
   TTY_TPB_Byte : Byte with Address => 16#FF76#;

   procedure Wait;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      TTY_TPB_Byte := 10;
      while TTY_TPS < 128 loop
         null;
      end loop;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Ch : Character) is
   begin
      TTY_TPB_Char := Ch;
      while TTY_TPS < 128 loop
         null;
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Ch : Character) is
   begin
      Put (Ch);
      New_Line;
   end Put_Line;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      while TTY_TPS < 128 loop
         null;
      end loop;
   end Wait;

end Ada.Text_IO;
