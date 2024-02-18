package body Minerva.Entries.Value.Objects is

   ---------------------------------
   -- Create_With_Dynamic_Address --
   ---------------------------------

   function Create_With_Dynamic_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Index;
      Address     : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address        => Address,
         Static_Address => 0,
         Local          => Offset,
         Variable_State => new Boolean'(False))
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type,
            Has_Address        => True,
            Has_Static_Address => False);
      end return;
   end Create_With_Dynamic_Address;

   ------------------------------
   -- Create_With_Local_Offset --
   ------------------------------

   function Create_With_Local_Offset
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Index)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address          => null,
         Static_Address   => 0,
         Local            => Offset,
         Variable_State   => null)
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type);
      end return;
   end Create_With_Local_Offset;

   --------------------------------
   -- Create_With_Static_Address --
   --------------------------------

   function Create_With_Static_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Address     : Minerva.Target.Target_Address_Type)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address        => null,
         Static_Address => Address,
         Local            => 1,
         Variable_State   => null)
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type,
            Has_Address        => True,
            Has_Static_Address => True);
      end return;
   end Create_With_Static_Address;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      if This.Has_Static_Address then
         Code.Push_Constant (Tagatha.Word_64 (This.Static_Address));
         Code.Pop_Indirect (This.Entry_Type.Content);
      elsif This.Has_Address and then This.Variable_State.all then
         Code.Push_Local (This.Local);
         Code.Pop_Indirect (This.Entry_Type.Content);
      else
         Code.Pop_Local (This.Local, This.Entry_Type.Content);
         if This.Variable_State /= null then
            This.Variable_State.all := True;
         end if;
      end if;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      if This.Has_Static_Address then
         Code.Push_Constant (Tagatha.Word_64 (This.Static_Address));
         Code.Dereference (This.Entry_Type.Content, 0);
      elsif This.Has_Address then
         Code.Push_Local (This.Local);
         Code.Dereference (This.Entry_Type.Content, 0);
      else
         Code.Push_Local (This.Local, This.Entry_Type.Content);
      end if;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      if This.Has_Static_Address then
         Code.Push_Constant (Tagatha.Word_64 (This.Static_Address));
      elsif This.Has_Address then
         Code.Push_Local (This.Local);
      else
         raise Constraint_Error with
           "cannot push address of local "
           & Minerva.Names.Cased_Text (This.Name);
      end if;
   end Push_Address;

end Minerva.Entries.Value.Objects;
