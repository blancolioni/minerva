with Minerva.Types;

package Minerva.Trees.Types is

   subtype Parent is Trees.Instance;

   type Instance is abstract new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Get_Type (This : Class) return Minerva.Types.Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent with
      record
         Tree_Type : Minerva.Types.Class_Reference := null;
      end record;

   function Get_Type (This : Class) return Minerva.Types.Class_Reference
   is (This.Tree_Type);

end Minerva.Trees.Types;
