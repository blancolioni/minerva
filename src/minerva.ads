package Minerva with Pure is

   type Argument_Mode is
     (In_Mode, Out_Mode, In_Out_Mode, Access_Mode);

   type Data_Type is (Untyped_Data, Floating_Point_Data);
   type Size_Bits_Range is range 0 .. 65535;

end Minerva;
