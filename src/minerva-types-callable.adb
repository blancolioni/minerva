package body Minerva.Types.Callable is

   ------------
   -- Create --
   ------------

   function Create
     (Definition       : not null access Minerva.Trees.Class;
      Return_Type      : Minerva.Types.Class_Reference;
      Formal_Arguments : Formal_Argument_Array)
      return Class_Reference
   is
      This : Instance := Instance'
        (Definition        => Minerva.Trees.Class_Reference (Definition),
         Has_Return_Type   => True,
         Return_Type       => Return_Type,
         Formal_Arguments  => <>,
         Object_Size       => Minerva.Target.Target_Address_Size,
         others            => <>);
   begin
      for Formal_Argument of Formal_Arguments loop
         This.Formal_Arguments.Append (Formal_Argument);
      end loop;
      return new Instance'(This);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Definition       : Minerva.Trees.Class_Reference;
      Formal_Arguments : Formal_Argument_Array)
      return Class_Reference
   is
      This : Instance := Instance'
        (Definition        => Definition,
         Has_Return_Type   => False,
         Return_Type       => null,
         Formal_Arguments  => <>,
         Object_Size       => Minerva.Target.Target_Address_Size,
         others            => <>);
   begin
      for Formal_Argument of Formal_Arguments loop
         This.Formal_Arguments.Append (Formal_Argument);
      end loop;
      return new Instance'(This);
   end Create;

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (This : Instance)
      return String
   is
      function Argument_Image (Index : Positive) return String
      is (if Index > This.Formal_Arguments.Last_Index
          then (if Index = 1 then "()" else ")")
          else (if Index = 1 then "(" else ",")
          & This.Formal_Arguments.Element (Index).Entry_Type.Short_Name
          & Argument_Image (Index + 1));

   begin
      if This.Has_Return_Type then
         return Argument_Image (1) & " -> "
           & This.Return_Type.Short_Name;
      else
         return Argument_Image (1);
      end if;
   end Description;

   -----------------------
   -- Is_Convertible_To --
   -----------------------

   overriding function Is_Convertible_To
     (From : not null access constant Instance;
      To   : not null access constant Minerva.Types.Class)
      return Boolean
   is
   begin
      return Minerva.Types.Class_Reference (From)
        = Minerva.Types.Class_Reference (To)
        or else (From.Has_Return_Type
                 and then From.Formal_Arguments.Is_Empty
                 and then From.Return_Type.Is_Convertible_To (To));
   end Is_Convertible_To;

end Minerva.Types.Callable;
