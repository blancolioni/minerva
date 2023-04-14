package Athena is

   type Vector is record
         X, Y, Z : Float := 0.0;
      end record;

   function Zero return Vector;

   function "+" (Left, Right : Vector) return Vector;
   function "-" (Left, Right : Vector) return Vector;

   function "*" (Left : Float; Right : Vector) return Vector;
   
end Athena;
