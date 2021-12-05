--  Test record type component references
with Minerva.Tests;

procedure Test_Record_2 is
   type Point is 
      record
         X, Y : Integer;
      end record;

   P : Point;
  
begin
   P.X := 1;
   P.Y := 2;
   Minerva.Tests.Result (P.X = 1);
end Test_Record_2;
