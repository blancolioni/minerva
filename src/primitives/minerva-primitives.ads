with Minerva.Environment;
with Minerva.Trees;
with Minerva.Types;

private package Minerva.Primitives is

   procedure Create_Primitives;

   function Package_Standard
     return Minerva.Trees.Class_Reference;

   function Standard_Environment
     return Minerva.Environment.Environment_Id;

   function Standard_Boolean
     return Minerva.Types.Class_Reference;

   function Standard_Character
     return Minerva.Types.Class_Reference;

   function Standard_Integer
     return Minerva.Types.Class_Reference;

   function System_Address
     return Minerva.Types.Class_Reference;

end Minerva.Primitives;
