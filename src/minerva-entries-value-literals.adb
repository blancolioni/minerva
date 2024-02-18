package body Minerva.Entries.Value.Literals is

   ------------
   -- Create --
   ------------

   function Create
     (Declaration   : not null access Minerva.Trees.Class;
      Literal_Name  : Minerva.Names.Minerva_Name;
      Literal_Type  : Minerva.Types.Class_Reference;
      Literal_Value : Natural)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with Value => Literal_Value)
      do
         This.Initialize_Entry
           (Declared_Name      => Literal_Name,
            Declaration        => Declaration,
            Entry_Type         => Literal_Type,
            Is_Overloadable    => True,
            Is_Constant        => True,
            Is_Static          => True,
            Is_Intrinsic       => True);
      end return;
   end Create;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with
        "cannot pop an enumeration literal";
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      Code.Push_Constant (Tagatha.Int_32 (This.Value));
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with
        "cannot push the address of an enumeration literal";
   end Push_Address;

end Minerva.Entries.Value.Literals;
