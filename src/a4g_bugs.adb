----------------------------------------------------------------------
--  A4G_Bugs - Package body                                         --
--  Copyright (C) 2005 Adalog                                       --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------

--## RULE OFF Use_A4G_Bugs ## We obviously need to call the original function here

with   -- ASIS
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions;

with   -- Adalog
  Thick_Queries,
  Utilities;   -- Only for Trace_Bug

package body A4G_Bugs is
   use Asis;

   -----------------------------------
   -- Corresponding_Expression_Type --
   -----------------------------------

   function Corresponding_Expression_Type (Expression : in Asis.Expression) return Asis.Declaration
   is
      use Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Result : Asis.Element;
   begin
      Result := Asis.Expressions.Corresponding_Expression_Type (Expression);

      if Is_Nil (Result) then
         -- There are cases where Corresponding_Expression_Type returns a wrong Nil result
         -- for the selector of a Selected_Component, but is OK on the Selected_Component itself.
         -- Let's give it a chance...
         if Expression_Kind (Enclosing_Element (Expression)) = A_Selected_Component
           and then Is_Equal (Expression, Selector (Enclosing_Element (Expression)))
         then
            Result := Asis.Expressions.Corresponding_Expression_Type (Enclosing_Element (Expression));
         end if;
         if Is_Nil (Result) then
            -- Must be true this time...
            return Result;
         end if;
         Trace_Bug ("A4G_Bugs.Corresponding_Expression_Type (1)");
      end if;

      case Declaration_Kind (Result) is
         when A_Component_Declaration =>
            -- Bug
            Trace_Bug ("A4G_Bugs.Corresponding_Expression_Type (2)");
            Result := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                      (Component_Subtype_Indication
                                                       (Object_Declaration_View (Result))));

         when A_Type_Declaration | A_Subtype_Declaration | A_Formal_Type_Declaration =>
            -- OK
            null;

         when A_Variable_Declaration | A_Constant_Declaration | A_Deferred_Constant_Declaration =>
            -- Bug
            Trace_Bug ("A4G_Bugs.Corresponding_Expression_Type (3)");
            Result := Object_Declaration_View (Result);
            if Definition_Kind (Result) = A_Type_Definition then
               -- The object is declared with an anonymous type
               -- => there is no corresponding declaration
               return Nil_Element;
            end if;

            Result := Corresponding_Name_Declaration (Subtype_Simple_Name (Result));

         when Not_A_Declaration =>
            raise Program_Error
              with "Bug in Corresponding_Expression_Type, returned "
                & Element_Kinds'Image (Element_Kind (Result));
         when others =>
            raise Program_Error
              with "Bug in Corresponding_Expression_Type, returned "
                & Declaration_Kinds'Image (Declaration_Kind (Result));
      end case;
      return Result;
   end Corresponding_Expression_Type;


   ---------------
   -- Trace_Bug --
   ---------------

   procedure Trace_Bug (Message : Wide_String) is
      use Utilities;
   begin
      Trace ("ASIS bug workaround triggered in " & Message);  --## RULE LINE OFF No_Trace
   end Trace_Bug;

end A4G_Bugs;
