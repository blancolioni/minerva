--  Test record type component references
with Minerva.Tests;

procedure Test_Record_2 is
   type Point is 
      record
         X, Y : Integer;
      end record;

   Local_Point : Point;
  
begin
   Local_Point.X := 1;
   Local_Point.Y := 2;
   Minerva.Tests.Result (Local_Point.X = 1);
end Test_Record_2;
