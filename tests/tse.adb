-- Test for the static evaluator

with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Text_IO;

with   -- ASIS components
  Asis.Ada_Environments,
  Asis.Compilation_Units,
  Asis.Elements,
  Asis.Errors,
  Asis.Exceptions,
  Asis.Implementation,
  Asis.Iterator,
  Asis.Text;

with   -- Other reusable components
  Implementation_Options,
  Options_Analyzer,
  Utilities,
  Thick_Queries;

procedure Tse is
   use Asis;
   use Compilation_Units, Elements;

   package Options is new Options_Analyzer (Binary_Options => "dhsb",
                                            Valued_Options => "p");
   use Options;

   procedure Print_Help is
      use Ada.Wide_Text_IO;
   begin
      Put_Line ("TSE");
      Put_Line ("Usage: tse [-ds] [-p p<roject_file> <unit> -- <ASIS_Options>");
      Put_Line ("   or: tse -h");
   end Print_Help;

   --------------------------------------------------------------------------
   -- The analyzer                                                         --
   --------------------------------------------------------------------------

   type Info is null record;

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info)
   is
      pragma Unreferenced (State);
      use Ada.Wide_Text_IO, Thick_Queries, Asis.Text, Ada.Strings, Ada.Strings.Wide_Fixed;
   begin
      case Element_Kind (Element) is
         when An_Expression =>
            Put (Trim (Element_Image (Element), Both));
            Put (" => ");
            Put_Line (Static_Expression_Value_Image (Element));
            Control := Abandon_Children;
         when others =>
            null;
      end case;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;

   procedure Traverse is new Asis.Iterator.Traverse_Element
     (Info, Pre_Procedure, Post_Procedure);

   My_Context     : Context;
   My_Unit        : Compilation_Unit;
   My_Declaration : Declaration;
   The_Control    : Traverse_Control := Continue;
   The_Info       : Info;

   use Ada.Wide_Text_IO, Ada.Characters.Handling, Implementation_Options;
begin
   if Is_Present (Option => 'h') then
      Print_Help;
      return;
   end if;

   if Is_Present (Option => 'd') then
      Utilities.Debug_Option := True;
   end if;

   if Parameter_Count /= 1 then
      Print_Help;
      return;
   end if;

   Implementation.Initialize;
   Ada_Environments.Associate (My_Context, "Ptree",
                               Parameters_String (Value (Option            => 'p',
                                                         Explicit_Required => True),
                                                  To_Wide_String (Options.Tail_Value)));
   Ada_Environments.Open (My_Context);

   declare
      Unit_Name : constant Wide_String := To_Wide_String (Parameter (1));
   begin
      if Is_Present (Option => 's') then
         My_Unit := Library_Unit_Declaration (Unit_Name, My_Context);
      else
         My_Unit := Compilation_Unit_Body (Unit_Name, My_Context);
      end if;
   end;

   declare
      My_CC_List : constant Context_Clause_List
        := Context_Clause_Elements (Compilation_Unit => My_Unit,
                                    Include_Pragmas  => True) ;
   begin
      for I in My_CC_List'Range loop
         Traverse (My_CC_List (I), The_Control, The_Info);
      end loop;
   end;

   My_Declaration := Unit_Declaration (My_Unit);
   Traverse (My_Declaration, The_Control, The_Info);

   Ada_Environments.Close (My_Context);
   Ada_Environments.Dissociate (My_Context);
   Implementation.Finalize;
exception
   when Occur : Asis.Exceptions.ASIS_Failed =>
      case Asis.Implementation.Status is
         when Asis.Errors.Use_Error =>
            Ada.Wide_Text_IO.Put_Line ("Inconsistent tree, please remove *.adt files");
         when others =>
            Ada.Wide_Text_IO.Put_Line (To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      end case;

   when Occur : Options_Error =>
      Ada.Wide_Text_IO.Put_Line (To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      Print_Help;
end Tse;
