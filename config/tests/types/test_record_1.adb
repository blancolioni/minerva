with Minerva.Tests;

procedure Test_Record_1 is
   type Point is 
      record
         X, Y : Integer;
      end record;

   P : Point;
  
begin
   Minerva.Tests.Result (True);
end Test_Record_1;
