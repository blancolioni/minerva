package body Minerva.Types is

   ---------------
   -- Data_Type --
   ---------------

   function Data_Type (This : Class) return Tagatha.Tagatha_Data_Type is
   begin
      if This.Is_Floating_Point then
         return Tagatha.Floating_Point_Data;
      else
         return Tagatha.Untyped_Data;
      end if;
   end Data_Type;

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

   ----------
   -- Size --
   ----------

   function Size
     (This : Class)
      return Tagatha.Tagatha_Size
   is
   begin
      return Tagatha.Bits_To_Size (This.Size_Bits);
   end Size;

end Minerva.Types;
