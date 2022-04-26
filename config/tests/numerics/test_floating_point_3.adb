with Minerva.Tests;

procedure Test_Floating_Point_3 is
   X : Float := 1.0;
begin
   if X < 1.1 then
      Minerva.Tests.Result (True);
   else
      Minerva.Tests.Result (False);
   end if;
end Test_Floating_Point_3;