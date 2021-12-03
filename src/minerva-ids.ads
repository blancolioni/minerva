private package Minerva.Ids with Pure is

   type Environment_Id_Base is new Natural;
   Null_Environment_Id : constant Environment_Id_Base := 0;
   subtype Environment_Id is
     Environment_Id_Base range 1 .. Environment_Id_Base'Last;

end Minerva.Ids;
