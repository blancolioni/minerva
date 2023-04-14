with Minerva.Tests;

procedure Test_Declare_1 is
   X : Integer := 4;
begin
   declare
       Y : Integer;
   begin
       Y := 3;
   end;
   Minerva.Tests.Result (X = 4);
end Test_Declare_1;