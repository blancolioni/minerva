package body Minerva.Types is

   -----------------------
   -- Is_Convertible_To --
   -----------------------

   function Is_Convertible_To
     (From : not null access constant Instance;
      To   : not null access constant Class) return Boolean
   is
      Result : constant Boolean :=
                 Class_Reference (From) = Class_Reference (To);
   begin
      return Result;
   end Is_Convertible_To;

end Minerva.Types;
