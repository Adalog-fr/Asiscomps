-- Ada
with
  Ada.Wide_Text_IO;

-- ASIS
with
  Asis.Ada_Environments,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Implementation,
  Asis.Iterator;

-- Adalog
with
  A4G_Bugs;
procedure Ta4g_bugs is
   use Ada.Wide_Text_Io;

   type Info is null record;

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info)
   is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
   begin
      case Element_Kind (Element) is
         when An_Expression =>
            if not Is_Equal (        A4G_Bugs.Corresponding_Expression_Type (Element),
                                     Asis.Expressions.Corresponding_Expression_Type (Element))
            then
               Put_Line ("Bug in Corresponding_Expression_Type");
            end if;

         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Procedure_Declaration =>
                  if Element_Kind (Corresponding_Body (Element)) /= A_Pragma then
                     Put_Line ("Bug in Corresponding_Body of interfaced SP");
                  end if;
               when A_Procedure_Renaming_Declaration | A_Function_Renaming_Declaration =>
                  if Expression_Kind (Asis.Declarations.Renamed_Entity (Element)) /= An_Attribute_Reference then
                     Put_Line ("Bug in Renamed_Entity");
                  end if;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
   begin
      null;
   end Post_Procedure;

   procedure Traverse is new Asis.Iterator.Traverse_Element
     (Info, Pre_Procedure, Post_Procedure);


   My_Context : Asis.Context;
   My_Unit : Asis.Compilation_Unit;

   use Asis.Compilation_Units, Asis.Elements;
   The_Control : Asis.Traverse_Control := Asis.Continue;
   The_Info    : Info;
begin
   --
   -- Initialize ASIS
   --
   Asis.Implementation.Initialize ("-ws -k");
   Asis.Ada_Environments.Associate (My_Context, "TA4G_Bugs", "-FM -CA");
   Asis.Ada_Environments.Open (My_Context);

   Put_Line ("ASIS version: " & Asis.Implementation.ASIS_Implementor_Version);

   --
   -- Process unit
   --

   My_Unit := Compilation_Unit_Body ("TA4G_Bugs_Essai", My_Context);

Process_Context_Clauses :
   declare
      My_CC_List : constant Asis.Context_Clause_List
        := Context_Clause_Elements (Compilation_Unit => My_Unit,
                                    Include_Pragmas  => True) ;
   begin
      for I in My_CC_List'Range loop
         Traverse (My_CC_List (I), The_Control, The_Info);
      end loop;
   end Process_Context_Clauses;

Process_Unit :
   declare
      My_Declaration : constant Asis.Declaration := Unit_Declaration (My_Unit);
   begin
      Traverse (My_Declaration, The_Control, The_Info);
   end Process_Unit;

   --
   -- Clean up ASIS
   --
   Asis.Ada_Environments.Close (My_Context);
   Asis.Ada_Environments.Dissociate (My_Context);
   Asis.Implementation.Finalize;

end TA4g_Bugs;
