private with Minerva.Names;
with Minerva.Trees.Types;

package Minerva.Trees.Declarations.Types is

   subtype Parent is Declarations.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create
     (Position         : GCS.Positions.File_Position;
      Defining_Name    : String;
      Type_Definition  : not null access
        Minerva.Trees.Types.Class)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Type_Name        : Minerva.Names.Minerva_Name;
         Type_Definition  : Minerva.Trees.Types.Class_Reference;
      end record;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Children (This : Instance) return Class_Reference_Array;

   overriding function Image (This : Instance) return String;

end Minerva.Trees.Declarations.Types;
