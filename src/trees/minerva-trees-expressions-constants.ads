private with Minerva.Ids;

package Minerva.Trees.Expressions.Constants is

   subtype Parent is Expressions.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Universal_Integer
     (Position : GCS.Positions.File_Position;
      Image    : String)
      return Class_Reference;

   function Create_Universal_Float
     (Position : GCS.Positions.File_Position;
      Image    : String)
      return Class_Reference;

private

   type Constant_Type is
     (Universal_Integer_Constant,
      Universal_Float_Constant,
      Universal_String_Constant);

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Const_Type : Constant_Type;
         Image      : Minerva.Names.Minerva_Name;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Children
     (This : Instance)
      return Class_Reference_Array;

   overriding procedure Set_Available_Types
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id);

   overriding procedure Constrain_Type
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List);

   overriding function Is_Static
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Boolean;

   overriding function Evaluate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Minerva.Values.Minerva_Value;

end Minerva.Trees.Expressions.Constants;
