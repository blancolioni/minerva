package Minerva.Types.Composite is

   subtype Parent is Types.Instance;

   type Instance is abstract new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access constant Instance;
   type Class_Reference is access constant Class;

   function Component_Environment
     (This : Class)
      return Minerva.Ids.Environment_Id;

   procedure Initialize
     (This : in out Class;
      Definition            : not null access Minerva.Trees.Class;
      Object_Size           : Minerva.Target.Object_Size;
      Component_Environment : Minerva.Ids.Environment_Id);

   function To_Composite
     (Base_Type : not null access constant Minerva.Types.Class)
      return Class_Reference
     with Pre => Base_Type.Is_Composite;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent with
      record
         Components : Minerva.Ids.Environment_Id;
      end record;

   function Component_Environment
     (This : Class)
      return Minerva.Ids.Environment_Id
   is (This.Components);

   function To_Composite
     (Base_Type : not null access constant Minerva.Types.Class)
      return Class_Reference
   is (Class_Reference (Base_Type));

end Minerva.Types.Composite;
