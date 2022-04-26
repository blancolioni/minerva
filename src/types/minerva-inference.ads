with Minerva.Ids;
with Minerva.Types.Lists;

private package Minerva.Inference is

   type Typeable_Interface is interface;

   function Image
     (This : Typeable_Interface)
      return String
      is abstract;

   function Get_Possible_Types
     (This : Typeable_Interface)
      return Minerva.Types.Lists.List
      is abstract;

   procedure Set_Type
     (This    : in out Typeable_Interface;
      To_Type : Minerva.Types.Class_Reference)
   is abstract;

   type Typeable_Reference is access all Typeable_Interface'Class;

   type Typeable_Reference_Array is
     array (Positive range <>) of Typeable_Reference;

   function Is_Leaf
     (This : Typeable_Interface)
      return Boolean
      is abstract;

   function Children
     (This : Typeable_Interface)
      return Typeable_Reference_Array
      is abstract;

   procedure Infer_Type
     (Root_Node     : not null access Typeable_Interface'Class;
      Expected_Type : Minerva.Types.Lists.List;
      Environment   : Minerva.Ids.Environment_Id);

end Minerva.Inference;
