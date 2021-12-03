package Minerva.Target is

   type Object_Size is range 0 .. 65535;

   type Target_Address_Type is mod 2 ** 16;
   Target_Address_Size : constant Object_Size := 16;
   Target_Word_Size    : constant Object_Size := 16;

   function To_Word_Size
     (Size : Object_Size)
      return Natural
   is (Natural ((Size + 15) / Target_Word_Size));

end Minerva.Target;
