with Minerva.Tests;

procedure Test_Floating_Point_3 is
   X : Float := 1.0;
begin
   X := X / 10.0;
   Minerva.Tests.Result (X = 0.1);
end Test_Floating_Point_3;