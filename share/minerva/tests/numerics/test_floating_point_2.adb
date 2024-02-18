with Minerva.Tests;

procedure Test_Floating_Point_2 is
   X : Float := 1.0;
begin
   Minerva.Tests.Result (X < 1.1);
end Test_Floating_Point_2;