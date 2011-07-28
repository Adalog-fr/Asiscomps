with Ada.Streams;
procedure Ta4g_Bugs_Essai is
   type Int_Ptr is access Integer;
   P : Int_Ptr;
   I : Integer;

   function Fun (X : Integer) return Integer renames Integer'Succ;
   procedure Proc (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Integer)
     renames Integer'Write;
   procedure Imported;
   pragma Import (C, Imported);

begin
   if P.all = 2 then
      null;
   end if;
end Ta4g_Bugs_Essai;
