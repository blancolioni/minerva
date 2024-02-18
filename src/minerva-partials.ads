with Tagatha.Code;

package Minerva.Partials is

   type Instance is abstract tagged private;
   type Reference is access constant Instance'Class;

   procedure Push
     (Partial : Instance;
      Code    : in out Tagatha.Code.Instance'Class)
   is abstract;

   procedure Pop
     (Partial : Instance;
      Code    : in out Tagatha.Code.Instance'Class)
   is abstract;

   function Local
     (Index : Tagatha.Local_Index)
      return Reference;

   function Argument
     (Index : Tagatha.Argument_Index)
      return Reference;

   function Constant_Value
     (Value : Tagatha.Word_64)
      return Reference;

   function Constant_Value
     (Value : Tagatha.Floating_Point_Constant)
      return Reference;

   function Offset
     (This : Instance)
      return Natural;

   function Add_Offset
     (This  : Instance;
      Words : Natural)
      return Reference;

private

   type Instance is abstract tagged
      record
         Offset : Natural := 0;
      end record;

   function Offset
     (This : Instance)
      return Natural
   is (This.Offset);

end Minerva.Partials;
