procedure Example_6 is
   Counter : Integer;
   Ch      : Byte;
   Tty_Enable : Byte with Address => 65408;
   Tty_Buffer : Byte with Address => 65409;
   
begin
   Counter := 95;
   Ch := 32;
   while Counter > 0 loop
      Tty_Buffer := Ch;
      Tty_Enable := 1;
      Counter := Counter - 1;
      Ch := Ch + 1;
   end loop;
   Tty_Buffer := 10;
   Tty_Enable := 1;
end Example_6;

   