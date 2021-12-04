with Minerva.Tests;

procedure Test_Floating_Point_1 is
   X : Float := 1.0;
   B : Boolean := X = 1.0;
begin
   Minerva.Tests.Result (B);
end Test_Floating_Point_1;