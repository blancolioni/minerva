private with Minerva.Ids;

package Minerva.Trees.Expressions.Dots is

   subtype Parent is Expressions.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Dot_Expression
     (Position   : GCS.Positions.File_Position;
      Left       : Expressions.Class_Reference;
      Right      : String)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Left      : Expressions.Class_Reference;
         Right     : Minerva.Names.Minerva_Name;
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

   overriding procedure Set_Available_Types
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id);

   overriding procedure Constrain_Type
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List);

   overriding procedure Set_Type
     (This          : in out Instance;
      Possible_Type : Minerva.Types.Class_Reference);

   overriding function Is_Static
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Boolean;

   overriding function Evaluate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Minerva.Values.Minerva_Value;

   overriding function Children
     (This : Instance)
      return Class_Reference_Array;

end Minerva.Trees.Expressions.Dots;
