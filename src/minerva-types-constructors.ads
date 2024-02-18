package Minerva.Types.Constructors is

   function Create_Signed_Integer_Type
     (Definition  : not null access Minerva.Trees.Class;
      Name        : String;
      First, Last : Integer)
      return Class_Reference;

   function Create_Modular_Type
     (Definition  : not null access Minerva.Trees.Class;
      Name        : String;
      Modulus     : Positive)
      return Class_Reference;

   function Single_Precision_Float_Type
      return Class_Reference;

end Minerva.Types.Constructors;
