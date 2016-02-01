----------------------------------------------------------------------
--  Units_List - Package specification                              --
--  Copyright (C) 2002-2016 Adalog                                  --
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

with Asis;
package Units_List is

   procedure Initialize (Context : access Asis.Context);

   -- Register unit specification
   -- Syntax : <unit>{+|-<unit>} | @<file>
   type Recursion_Mode is (None, Spec_Only, Spec_Closure, Full);
   -- None:         Don't consider any with clause
   -- Spec_Only:    Consider (recursively) only with clauses on specifications
   -- Spec_Closure: Like Spec_Only, + with clauses of bodies of units given in Unit_Spec
   -- Full:         Consider (recursively) all with clauses
   procedure Register (Units_Spec : in Wide_String;
                       Recursion  : in Recursion_Mode;
                       Add_Stubs  : in Boolean);
   Specification_Error : exception;

   -- List iterator:
   procedure Reset;
   procedure Skip;
   function Is_Exhausted return Boolean;

   type Unit_Order is private;
   function "<=" (Left, Right : Unit_Order) return Boolean;
   -- Order of addition of units to the list

   function Length        return Natural;
   function Current_Unit  return Wide_String;
   function Current_Order return Unit_Order;
   function Last_Order    return Unit_Order;
   procedure Delete_Current;

private
   type Unit_Order is new Natural;

end Units_List;
