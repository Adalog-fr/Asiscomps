----------------------------------------------------------------------
--  Elements_Set - Package body                                     --
--  Copyright (C) 2012 Adalog                                       --
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

with
  Thick_Queries, Utilities;
package body Elements_Set is
   use Thick_Queries;

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Set; Element : Asis.Element) is
      use Utilities;
   begin
      Add (To,
           To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Simple_Name (Element)))),
           First_Defining_Name (Element));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Set; Elements : Asis.Element_List) is
   begin
      for E in Elements'Range loop
         Add (To, Elements (E));
      end loop;
   end Add;

   ---------------------
   -- Elements_In_Set --
   ---------------------

   function Elements_In_Set (S : Set) return Asis.Element_List is
      use Asis;
      use Element_Map;

      Local_Map : Map := Map (S);

      Count : Asis_Natural := 0;
      procedure Inc  (Key : Unbounded_Wide_String; Value : in out Asis.Element) is
         pragma Unreferenced (Key, Value);
      begin
         Count := Count + 1;
      end Inc;
      procedure Count_Set is new Iterate (Inc);
   begin
      Count_Set (Local_Map);
      declare
         Result : Element_List (1 .. Count);
         Inx    : Asis_Natural := 0;
         procedure Add_One (Key : Unbounded_Wide_String; Value : in out Asis.Element) is
            pragma Unreferenced (Key);
         begin
            Inx := Inx + 1;
            Result (Inx) := Value;
         end Add_One;
         procedure Populate is new Iterate (Add_One);
      begin
         Populate (Local_Map);
         return Result;
      end;
   end Elements_In_Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (The_Set : in out Set) is
      use Element_Map;
   begin
      Clear (Map (The_Set));
   end Clear;
end Elements_Set;
