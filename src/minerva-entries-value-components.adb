package body Minerva.Entries.Value.Components is

   ------------
   -- Create --
   ------------

   function Create
     (Declaration    : not null access Minerva.Trees.Class;
      Component_Name : Minerva.Names.Minerva_Name;
      Component_Type : Minerva.Types.Class_Reference;
      Word_Offset    : Natural;
      Bit_Offset     : Natural := 0)
      return Constant_Class_Reference
   is
      This : Instance := Instance'
        (Parent with
         Word_Offset => Word_Offset,
         Bit_Offset  => Bit_Offset);
   begin
      Initialize_Value_Entry
        (This               => This,
         Declared_Name      => Component_Name,
         Declaration        => Declaration,
         Entry_Type         => Component_Type);
      return new Instance'(This);
   end Create;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      Code.Pop_Indirect (Offset => Tagatha.Int_32 (This.Word_Offset));
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      Code.Dereference (Offset => Tagatha.Int_32 (This.Word_Offset));
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
      use Minerva.Target;
      Offset : constant Natural :=
                 This.Word_Offset
                   * Natural (Target_Word_Size / 8);
   begin
      Code.Push_Constant
        (Tagatha.Int_32 (Offset));
      Code.Operate (Tagatha.Op_Add);
   end Push_Address;

end Minerva.Entries.Value.Components;
