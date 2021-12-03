private with Minerva.Names;

package Minerva.Trees.Types.Named is

   subtype Parent is Types.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Named_Type
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Name : Minerva.Names.Minerva_Name;
      end record;

   overriding function Children (This : Instance) return Class_Reference_Array
   is (Null_Class_Reference_Array);

   overriding function Image (This : Instance) return String;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

end Minerva.Trees.Types.Named;
