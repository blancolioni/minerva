with Minerva.Names;

with Minerva.Environment;

with Minerva.Trees.Expressions;

package Minerva.Trees.Aspects is

   subtype Parent is Trees.Instance;

   type Instance is new Parent
     and Minerva.Names.Name_Interface
   with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Aspect
     (Position : GCS.Positions.File_Position;
      Name     : String;
      Value    : not null Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference;

   function Create_Aspect
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference;

   function Has_Value
     (Aspect : Class)
      return Boolean;

   function Value
     (Aspect : Class)
      return not null Minerva.Trees.Expressions.Class_Reference
     with Pre => Aspect.Has_Value;

private

   subtype Dispatch is Instance'Class;

   type Instance is
     new Parent
     and Minerva.Names.Name_Interface with
      record
         Name  : Minerva.Names.Minerva_Name;
         Value : Minerva.Trees.Expressions.Class_Reference;
      end record;

   overriding function Name
     (This : Instance)
      return Minerva.Names.Minerva_Name
   is (This.Name);

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

   overriding function Image
     (This : Instance)
      return String;

end Minerva.Trees.Aspects;
