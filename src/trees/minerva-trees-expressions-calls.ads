private with Ada.Containers.Vectors;
private with Minerva.Ids;
private with Minerva.Operators;

package Minerva.Trees.Expressions.Calls is

   subtype Parent is Expressions.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Actual_Argument_Array is
     array (Positive range <>) of Expressions.Class_Reference;

   function Create_Call_Expression
     (Position   : GCS.Positions.File_Position;
      Call       : not null Expressions.Class_Reference;
      Arguments  : Actual_Argument_Array)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   package Actual_Argument_Vectors is
     new Ada.Containers.Vectors (Positive, Expressions.Class_Reference,
                                 Expressions."=");

   type Instance is new Parent with
      record
         Call       : Expressions.Class_Reference;
         Actuals    : Actual_Argument_Vectors.Vector;
         Intrinsic  : Boolean := False;
         Operator   : Minerva.Operators.Minerva_Operator :=
                        Minerva.Operators.Op_None;
         Call_Types : Minerva.Types.Lists.List;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   --  overriding function Get_Possible_Types
   --    (This : Instance)
   --     return Minerva.Types.Lists.List;

   --  overriding function Children
   --    (This : Instance)
   --     return Minerva.Inference.Typeable_Reference_Array;

   overriding procedure Set_Type
     (This          : in out Instance;
      Possible_Type : not null Minerva.Types.Class_Reference);

   overriding function Constrain_Types
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
      return Minerva.Types.Lists.List;

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

end Minerva.Trees.Expressions.Calls;
