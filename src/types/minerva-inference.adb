with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed.Hash;

package body Minerva.Inference is

   Log_Unification : constant Boolean := False;

   type Annotation_Type is tagged
      record
         Index : Positive;
      end record;

   function Is_Leaf (Annotation : Annotation_Type'Class) return Boolean;
   function Show (Annotation : Annotation_Type'Class) return String;

   package Annotation_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Annotation_Type);

   function Children
     (Annotation : Annotation_Type'Class)
      return Annotation_Lists.List;

   type Annotation_Record is
      record
         Reference : Typeable_Reference;
         Is_Leaf   : Boolean := True;
         Variable  : Natural := 0;
         Possible  : Minerva.Types.Lists.List;
         Children  : Annotation_Lists.List;
      end record;

   type Array_Of_Annotations is
     array (Positive range <>) of Annotation_Type;

   package Annotation_Vectors is
     new Ada.Containers.Vectors (Positive, Annotation_Record);

   Annotation_Vector : Annotation_Vectors.Vector;

   function Annotate
     (Tree        : not null access Typeable_Interface'Class;
      Environment : Minerva.Ids.Environment_Id)
      return Annotation_Type;

   procedure Unify
     (Top      : Annotation_Type;
      Expected : Minerva.Types.Lists.List);

   package Binding_Vectors is
     new Ada.Containers.Vectors
       (Positive, Annotation_Type);

   Binding_Vector : Binding_Vectors.Vector;

   package Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Positive,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   package Set_Of_Names is
     new Ada.Containers.Indefinite_Hashed_Sets
       (Element_Type        => String,
        Hash                => Ada.Strings.Fixed.Hash,
        Equivalent_Elements => "=");

   package Dependency_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Set_Of_Names.Set,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => Set_Of_Names."=");

   package List_Of_Local_Bindings is
     new Ada.Containers.Doubly_Linked_Lists (Annotation_Type);

   package Local_Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => List_Of_Local_Bindings.List,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => List_Of_Local_Bindings."=");

   function Scan_Variable_Annotations
     (Tree : Typeable_Reference)
      return Array_Of_Annotations;

   function Unify
     (Left_Annotation  : Annotation_Type;
      Right_Annotation : Annotation_Type;
      Bindings         : in out Array_Of_Annotations)
      return Minerva.Types.Class_Reference;

   function Bind
     (Annotation    : Annotation_Type;
      Bindings      : in out Array_Of_Annotations;
      Next_Variable : in out Natural)
      return Minerva.Types.Class_Reference;

   function Dereference
     (Annotation : Annotation_Type;
      Bindings   : Array_Of_Annotations)
      return Annotation_Type;

   function Show
     (List : Annotation_Lists.List;
      Bindings : Array_Of_Annotations)
      return String;

   function Show_Dereferenced_Type
     (Annotation : Annotation_Type;
      Bindings   : Array_Of_Annotations)
      return String
   is (if Annotation.Is_Leaf
       then Dereference (Annotation, Bindings).Show
       else Show (Annotation.Children, Bindings));

   --------------
   -- Annotate --
   --------------

   function Annotate
     (Tree        : not null access Typeable_Interface'Class;
      Environment : Minerva.Ids.Environment_Id)
      return Annotation_Type
   is
      Rec : Annotation_Record := Annotation_Record'
        (Reference => Typeable_Reference (Tree),
         Is_Leaf   => Tree.Is_Leaf,
         Variable  => 0,
         Possible  => Tree.Get_Possible_Types,
         Children  => <>);
      Result : constant Annotation_Type :=
                 Annotation_Type'
                   (Index => Annotation_Vector.Last_Index + 1);
   begin
      Annotation_Vector.Append (Rec);
      if not Tree.Is_Leaf then
         for Child of Tree.Children loop
            Rec.Children.Append (Annotate (Child, Environment));
         end loop;
         Annotation_Vector (Result.Index) := Rec;
      end if;
      return Result;
   end Annotate;

   ----------------
   -- Infer_Type --
   ----------------

   procedure Infer_Type
     (Root_Node     : not null access Typeable_Interface'Class;
      Expected_Type : Minerva.Types.Lists.List;
      Environment   : Minerva.Ids.Environment_Id)
   is
      Top : constant Annotation_Type :=
              Annotate (Root_Node, Environment);
   begin
      Unify (Top, Expected_Type);
   end Infer_Type;

   procedure Unify
     (Top      : Annotation_Type;
      Expected : Minerva.Types.Lists.List)
   is
      Rec : Annotation_Record renames Annotation_Vector (Top.Index);
   begin
      Resolve (Expected, Rec.Possible);


end Minerva.Inference;
