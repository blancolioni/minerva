with Tagatha.Operands;

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

   ----------
   -- Push --
   ----------

   overriding function To_Operand
     (This : Instance)
      return Tagatha.Operands.Operand_Type
   is
   begin
      return Tagatha.Operands.Constant_Operand
        (Tagatha.Tagatha_Integer (This.Value));
   end To_Operand;

end Minerva.Entries.Value.Literals;
