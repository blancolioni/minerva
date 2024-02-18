with Minerva.Entries.Value.Literals;
with Minerva.Environment;
with Minerva.Names;
with Minerva.Target;

with Minerva.Types.Callable;

package body Minerva.Types.Enumerated is

   -----------------
   -- Add_Literal --
   -----------------

   procedure Add_Literal
     (This        : in out Class;
      Declaration : not null access Minerva.Trees.Class;
      Name        : String)
   is
   begin
      This.Add_Literal
        (Declaration, Name, Natural (This.Literals.Length));
   end Add_Literal;

   -----------------
   -- Add_Literal --
   -----------------

   procedure Add_Literal
     (This        : in out Class;
      Declaration : not null access Minerva.Trees.Class;
      Name        : String;
      Value       : Natural)
   is
      use Minerva.Target;
   begin
      This.Literals.Append
        (Literal_Record'
           (Declaration   => Minerva.Trees.Class_Reference (Declaration),
            Identifier    =>
              Minerva.Names.To_Name
                (Name, Case_Sensitive => Name (Name'First) = '''),
            Value         => Value));
      if 2 ** Natural (This.Object_Size) < Natural (This.Literals.Length) then
         This.Object_Size := This.Object_Size + 1;
      end if;
   end Add_Literal;

   ----------------------------
   -- Create_Enumerated_Type --
   ----------------------------

   function Create_Enumerated_Type
     (Definition : not null access Minerva.Trees.Class;
      Name       : String)
      return Class_Reference
   is
      This : constant Class_Reference := new Instance'
        (Parent with
         Literals     => Literal_Lists.Empty_List,
         others       => <>);
      No_Formal_Arguments : Minerva.Types.Callable
        .Formal_Argument_Array (1 .. 0);
   begin
      This.Definition := Minerva.Trees.Class_Reference (Definition);
      This.Name := Minerva.Names.To_Name (Name);
      This.Object_Size := 0;
      This.Is_Ordered := True;
      This.Literal_Type :=
        Types.Class_Reference
          (Minerva.Types.Callable.Create
             (Definition       => Minerva.Trees.Class_Reference (Definition),
              Return_Type      => Minerva.Types.Class_Reference (This),
              Formal_Arguments => No_Formal_Arguments));
      return This;
   end Create_Enumerated_Type;

   ---------------
   -- Elaborate --
   ---------------

   overriding procedure Elaborate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
   is
   begin
      for Literal of This.Literals loop
         Minerva.Environment.Insert
           (Environment,
            Minerva.Entries.Value.Literals.Create
              (Declaration   => Literal.Declaration,
               Literal_Name  => Literal.Identifier,
               Literal_Type  => This.Literal_Type,
               Literal_Value => Literal.Value));
      end loop;
   end Elaborate;

   ----------------
   -- Is_Literal --
   ----------------

   function Is_Literal
     (This    : Class;
      Literal : String)
      return Boolean
   is
      use type Minerva.Names.Minerva_Name;
   begin
      return (for some Literal_Entry of This.Literals =>
                Literal_Entry.Identifier = Literal);
   end Is_Literal;

   -------------------
   -- Literal_Value --
   -------------------

   function Literal_Value
     (This    : Class;
      Literal : String)
      return Natural
   is
      use type Minerva.Names.Minerva_Name;
   begin
      for Literal_Entry of This.Literals loop
         if Literal_Entry.Identifier = Literal then
            return Literal_Entry.Value;
         end if;
      end loop;
      raise Constraint_Error with
        "precondition violated";
   end Literal_Value;

end Minerva.Types.Enumerated;
