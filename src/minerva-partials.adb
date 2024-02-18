with Tagatha.Conversions;

package body Minerva.Partials is

   type Local_Instance is new Instance with
      record
         Index : Tagatha.Local_Index;
      end record;

   overriding procedure Push
     (This : Local_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Pop
     (This : Local_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   type Argument_Instance is new Instance with
      record
         Index : Tagatha.Argument_Index;
      end record;

   overriding procedure Push
     (This : Argument_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Pop
     (This : Argument_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   type Constant_Integral_Instance is new Instance with
      record
         Value : Tagatha.Word_64;
      end record;

   overriding procedure Push
     (This : Constant_Integral_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Pop
     (This : Constant_Integral_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   type Constant_Floating_Point_Instance is new Instance with
      record
         Value : Tagatha.Floating_Point_Constant;
      end record;

   overriding procedure Push
     (This : Constant_Floating_Point_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Pop
     (This : Constant_Floating_Point_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   ----------------
   -- Add_Offset --
   ----------------

   function Add_Offset
     (This  : Instance;
      Words : Natural)
      return Reference
   is
      This_Class : Instance'Class renames Instance'Class (This);
      Result     : Instance'Class := This_Class;
   begin
      Result.Offset := Result.Offset + Words;
      return new Instance'Class'(Result);
   end Add_Offset;

   --------------
   -- Argument --
   --------------

   function Argument
     (Index : Tagatha.Argument_Index)
      return Reference
   is
   begin
      return new Argument_Instance'(0, Index);
   end Argument;

   --------------------
   -- Constant_Value --
   --------------------

   function Constant_Value
     (Value : Tagatha.Word_64)
      return Reference
   is
   begin
      return new Constant_Integral_Instance'(0, Value);
   end Constant_Value;

   --------------------
   -- Constant_Value --
   --------------------

   function Constant_Value
     (Value : Tagatha.Floating_Point_Constant)
      return Reference
   is
   begin
      return new Constant_Floating_Point_Instance'(0, Value);
   end Constant_Value;

   -----------
   -- Local --
   -----------

   function Local
     (Index : Tagatha.Local_Index)
      return Reference
   is
   begin
      return new Local_Instance'(0, Index);
   end Local;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Argument_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
      use Tagatha;
   begin
      Code.Pop_Argument (This.Index + Argument_Count (This.Offset));
   end Pop;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Constant_Integral_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with
        "cannot pop constant integers";
   end Pop;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Constant_Floating_Point_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with
        "cannot pop constant floating points";
   end Pop;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Local_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
      use Tagatha;
   begin
      Code.Pop_Local (This.Index + Local_Count (This.Offset));
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Argument_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
      use Tagatha;
   begin
      Code.Push_Argument (This.Index + Argument_Count (This.Offset));
   end Push;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Constant_Integral_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      Code.Push_Constant (This.Value);
   end Push;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Constant_Floating_Point_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      Code.Push_Constant
        (Tagatha.Conversions.Floating_Point_To_Word_64 (This.Value));
   end Push;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Local_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
      use Tagatha;
   begin
      Code.Push_Local (This.Index + Local_Count (This.Offset));
   end Push;

end Minerva.Partials;
