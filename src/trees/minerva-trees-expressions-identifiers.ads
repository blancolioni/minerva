private with Minerva.Ids;

package Minerva.Trees.Expressions.Identifiers is

   subtype Parent is Expressions.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Identifier_Expression
     (Position   : GCS.Positions.File_Position;
      Identifier : Minerva.Names.Minerva_Name)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Identifier       : Minerva.Names.Minerva_Name;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding function Children
     (This : Instance)
      return Class_Reference_Array;

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Constrain_Types
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
      return Minerva.Types.Lists.List;

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

end Minerva.Trees.Expressions.Identifiers;
