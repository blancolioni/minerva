with Minerva.Tests;

procedure Test_Declare_1 is
   X : Integer := 1;
begin
   declare
       Y : Integer;
   begin
       Y := 3;
   end;
   Minerva.Tests.Result (X = 1);
end Test_Declare_1;