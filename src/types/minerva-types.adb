with Minerva.Logging;

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
      Minerva.Logging.Log
        ("is-convertible: "
         & Dispatch (From.all).Short_Name
         & " => "
         & To.Short_Name
         & " = "
         & Result'Image);
      return Result;
   end Is_Convertible_To;

end Minerva.Types;
