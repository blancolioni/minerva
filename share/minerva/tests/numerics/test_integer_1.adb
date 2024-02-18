with Minerva.Tests;

procedure Test_Integer_1 is
   X : Integer := 1;
   B : Boolean := X = 1;
begin
   Minerva.Tests.Result (B);
end Test_Integer_1;