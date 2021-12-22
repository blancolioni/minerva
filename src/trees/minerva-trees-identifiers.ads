with Minerva.Names;

package Minerva.Trees.Identifiers is

   subtype Parent is Trees.Instance;

   type Instance is
     new Parent
     and Minerva.Names.Name_Interface
   with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Identifier_Reference
     (Position   : GCS.Positions.File_Position;
      Identifier : String)
      return Class_Reference;

   function Defining_Identifier
     (Position   : GCS.Positions.File_Position;
      Identifier : String)
      return Class_Reference;

private

   type Instance is
     new Parent
     and Minerva.Names.Name_Interface with
      record
         Defining    : Boolean;
         Identifier  : Minerva.Names.Minerva_Name;
      end record;

   overriding function Name
     (This : Instance)
      return Minerva.Names.Minerva_Name
   is (This.Identifier);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Code.Instance);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

   overriding function Image
     (This : Instance)
      return String;

   function Identifier
     (This : Class)
      return Minerva.Names.Minerva_Name
   is (This.Identifier);

end Minerva.Trees.Identifiers;
