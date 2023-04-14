with Minerva.Tests;

procedure Test_If_1 is
   X : Integer := 1;
   B : Boolean := X = 2;
   C : Boolean := True;
begin
   if B then
      C := False;
   end if;
   Minerva.Tests.Result (C);
end Test_If_1;