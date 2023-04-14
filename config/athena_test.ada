with Athena;                           use Athena;

procedure Athena_Test is
   S : Float := 5.0;
   V : Vector;
begin
   V := Zero;
   V.X := 2.0;
   V := S * V;
end Athena_Test;
