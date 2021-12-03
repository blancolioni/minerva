with Minerva.Environment;
with Minerva.Trees.Expressions;
with Minerva.Trees.Identifiers.Sequence;
with Minerva.Trees.Types;

package Minerva.Trees.Declarations.Objects is

   subtype Parent is Declarations.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Object_Declaration
     (Position    : GCS.Positions.File_Position;
      Names       : Minerva.Trees.Identifiers.Sequence.Class_Reference;
      Object_Type : Minerva.Trees.Types.Class_Reference;
      Initializer : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference;

   function Create_Formal_Argument_Declaration
     (Position    : GCS.Positions.File_Position;
      Names       : Minerva.Trees.Identifiers.Sequence.Class_Reference;
      Mode        : Argument_Mode;
      Object_Type : Minerva.Trees.Types.Class_Reference;
      Initializer : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference;

private

   type Declaration_Context is
     (Package_Context,
      Block_Context,
      Formal_Argument_Context);

   type Instance is new Parent with
      record
         Context         : Declaration_Context;
         Names           : Minerva.Trees.Identifiers.Sequence.Class_Reference;
         Mode            : Argument_Mode;
         Object_Type     : Minerva.Trees.Types.Class_Reference;
         Has_Initializer : Boolean := False;
         Initializer     : Minerva.Trees.Expressions.Class_Reference;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

   overriding procedure Iterate_Entries
     (This    : Instance;
      Process : not null access
        procedure (Item : Minerva.Entries.Constant_Class_Reference));

end Minerva.Trees.Declarations.Objects;
