with Minerva.Tests;

procedure Test_Integer_1 is
   X : Integer := 22;
   B : Boolean := X = 22;
begin
   Minerva.Tests.Result (B);
end Test_Integer_1;