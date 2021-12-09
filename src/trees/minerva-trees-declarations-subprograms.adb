with Ada.Containers.Vectors;
with Ada.Tags;

with Minerva.Entries.Subprograms;
with Minerva.Entries.Value.Formal_Arguments;
with Minerva.Types.Void;
with Minerva.Types.Callable;

package body Minerva.Trees.Declarations.Subprograms is

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      if This.Has_Context then
         This.Context.Check (Environment);
      end if;

      This.Formal_Arguments.Check (This.Argument_Env);

      if This.Has_Body then
         This.Subprogram_Body.Check (This.Argument_Env);
      end if;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children
     (This : Instance)
      return Class_Reference_Array
   is
   begin

      return Maybe (This.Context)
        & Maybe (This.Formal_Arguments)
        & (if This.Is_Function
           then (1 => Trees.Class_Reference (This.Return_Type))
           else Null_Class_Reference_Array)
        & (if This.Has_Body
           then (1 => Trees.Class_Reference (This.Subprogram_Body))
           else Null_Class_Reference_Array);
   end Children;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      if This.Has_Body then
         Unit.Begin_Routine
           (Name           => This.Get_Entry.Link_Name,
            Argument_Words => 0,
            Result_Words   => 0,
            Global         => True);
         This.Subprogram_Body.Compile (Unit);
         Unit.End_Routine;
      end if;
   end Compile_Tree;

   ------------------------
   -- Create_Declaration --
   ------------------------

   function Create_Declaration
     (Position         : GCS.Positions.File_Position;
      Context          : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name    : String;
      Formal_Arguments : Objects.Sequence.Class_Reference;
      Return_Type      : Minerva.Trees.Types.Class_Reference;
      Subprogram_Body  : Minerva.Trees.Blocks.Class_Reference)
      return Class_Reference
   is
      use type Minerva.Trees.Blocks.Class_Reference;
   begin
      return Result : constant Class_Reference :=
        Create_Specification (Position, Context, Defining_Name,
                              Formal_Arguments, Return_Type)
      do
         Result.Has_Body := Subprogram_Body /= null;
         Result.Subprogram_Body := Subprogram_Body;
      end return;
   end Create_Declaration;

   --------------------------
   -- Create_Specification --
   --------------------------

   function Create_Specification
     (Position         : GCS.Positions.File_Position;
      Context          : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name    : String;
      Formal_Arguments : Objects.Sequence.Class_Reference;
      Return_Type      : Minerva.Trees.Types.Class_Reference)
      return Class_Reference
   is
      use type Minerva.Trees.Context.Sequence.Class_Reference;
      use type Minerva.Trees.Types.Class_Reference;
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
         Has_Body         => False,
         Is_Function      => Return_Type /= null,
         Has_Context      => Context /= null,
         Context          => Context,
         Defining_Name    => Minerva.Names.To_Name (Defining_Name),
         Argument_Env     => <>,
         Formal_Arguments => Formal_Arguments,
         Return_Type      => Return_Type,
         Subprogram_Body  => null)
      do
         Result.Initialize (Position);
      end return;
   end Create_Specification;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
      use type Minerva.Trees.Types.Class_Reference;
   begin
      if This.Has_Context then
         This.Context.Elaborate (Environment);
      end if;

      if This.Return_Type /= null then
         This.Return_Type.Elaborate (Environment);
      end if;

      This.Argument_Env :=
        Minerva.Environment.Create_Environment
          (Minerva.Names.To_Name
             (Minerva.Names.Standard_Text (This.Defining_Name)
              & "-arguments"),
           Environment);

      This.Formal_Arguments.Elaborate (This.Argument_Env);

      declare
         subtype Call_Reference is
           Minerva.Types.Callable.Class_Reference;
         subtype Formal_Argument_Array is
           Minerva.Types.Callable.Formal_Argument_Array;
         subtype Formal_Reference is
           Minerva.Entries.Value.Formal_Arguments.Constant_Class_Reference;

         Return_Type : constant Minerva.Types.Class_Reference :=
                         (if This.Return_Type = null
                          then Minerva.Types.Void.Void_Type
                          else This.Return_Type.Get_Type);

         package Formal_Argument_Vectors is
           new Ada.Containers.Vectors
             (Positive, Formal_Reference,
              Minerva.Entries.Value.Formal_Arguments."=");

         function Create_Formal_Arguments return Formal_Argument_Array;

         -----------------------------
         -- Create_Formal_Arguments --
         -----------------------------

         function Create_Formal_Arguments return Formal_Argument_Array is

            Formal_Vector : Formal_Argument_Vectors.Vector;

            procedure Add
              (Dec : Minerva.Trees.Declarations.Objects.Class_Reference);

            ---------
            -- Add --
            ---------

            procedure Add
              (Dec : Minerva.Trees.Declarations.Objects.Class_Reference)
            is
               procedure Inner_Add
                 (Formal_Entry : Minerva.Entries.Constant_Class_Reference);

               ---------------
               -- Inner_Add --
               ---------------

               procedure Inner_Add
                 (Formal_Entry : Minerva.Entries.Constant_Class_Reference)
               is
               begin
                  if Formal_Entry.all not in
                    Minerva.Entries.Value.Formal_Arguments.Class
                  then
                     raise Constraint_Error with
                       "expected a formal argument, but found "
                       & Ada.Tags.External_Tag
                       (Formal_Entry.all'Tag);
                  end if;

                  Formal_Vector.Append (Formal_Reference (Formal_Entry));
               end Inner_Add;

            begin
               Dec.Iterate_Entries (Inner_Add'Access);
            end Add;

         begin
            This.Formal_Arguments.Iterate (Add'Access);

            return Args : Formal_Argument_Array
              (1 .. Formal_Vector.Last_Index)
            do
               for I in Args'Range loop
                  Args (I) := Formal_Reference (Formal_Vector.Element (I));
               end loop;
            end return;
         end Create_Formal_Arguments;

         Formal_Arguments : constant Formal_Argument_Array :=
                              Create_Formal_Arguments;

         Call_Type : constant Call_Reference :=
                       Minerva.Types.Callable.Create
                         (Definition       => This,
                          Return_Type      => Return_Type,
                          Formal_Arguments => Formal_Arguments);
      begin
         This.Set_Entry
           (Minerva.Entries.Subprograms.Create
              (Declaration     => This,
               Subprogram_Name => This.Defining_Name,
               Call_Type       => Call_Type));
         Minerva.Environment.Insert (Environment, This.Get_Entry);
      end;

      if This.Has_Body then
         This.Subprogram_Body.Elaborate (This.Argument_Env);
      end if;

   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return (if This.Is_Function then "function " else "procedure ")
        & Minerva.Names.Cased_Text (This.Defining_Name);
   end Image;

end Minerva.Trees.Declarations.Subprograms;
