Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.EditorInput
Imports Autodesk.AutoCAD.Runtime
Imports System.IO
Imports Autodesk.AutoCAD.Geometry

Public Class RV_Tools

    Dim insPt As Point3d
    Dim doc As Document = Application.DocumentManager.MdiActiveDocument
    Dim db As Database = doc.Database
    Dim ed As Editor = doc.Editor

#Region "Titre"

    <CommandMethod("rvtitre")> Sub Titre()

        'Si le gabarit n'est pas bon, on sort et on termine la fonction
        Try
            Application.SetSystemVariable("TEXTSTYLE", "LEGENDE")
            Application.SetSystemVariable("TEXTSTYLE", "Titre")
            Application.SetSystemVariable("TEXTSTYLE", "Titre Gras")
        Catch Ex As Exception
            MsgBox("Veuillez utiliser un gabarit Renfer & Venant")
            Exit Sub
        End Try

        'On lance la fenêtre de configuration
        Dim objForm As New TitreForm
        Application.ShowModalDialog(objForm)

        'On gère si on a validé ou annuler
        If objForm.Label_Hidden.Text = "OK" Then
        Else
            Exit Sub
        End If

        'On insert le bloc de titre qui va bien
        InsererTitre(objForm.ComboBox_TypeTitre.Text, objForm.Combobox_Echelle.Text, objForm.TextBox_Dossier.Text, objForm.TextBox_Doc.Text, objForm.DateTimePicker1.Text, objForm.TextBox_Modif.Text)

        'Si c'est le titre de base, alors on insère le bloc ENTETE
        If objForm.ComboBox_TypeTitre.Text = "Titre de Base" Then
            Inserer_Block_TitredeBase(objForm.ComboBox_Commune.Text, objForm.Lbl_Departement.Text)
        End If

        'On insert ensuite le titre du type de plan
        Dim BlockTextName As String
        If objForm.ComboBox_Plans.Text.Contains(" / ") Then
            BlockTextName = "Titre (2 lignes)"
        Else
            BlockTextName = "Titre (1 ligne)"
        End If
        Inserer_TypePlan(objForm.ComboBox_Plans.Text, BlockTextName)

        'Insertion de logo
        Dim strLogo As String = ""
        If objForm.RadioButton_Col.Checked = True Then
            strLogo = "Logo Titre Renfer Venant (grand) 1000e"
        Else
            strLogo = "Logo Titre Renfer Venant St-Denis (Grand) 1000e"
        End If
        Inserer_Logo(strLogo)

        'Insertion des lignes descriptives
        If objForm.ComboBox_Descript.Text <> "" Then
            Dim intNbrDescript As Integer = objForm.ComboBox_Descript.Text.Substring(0, 1)
            Inserer_Descript(intNbrDescript)
        End If

        'Insertion des lignes informatives (Commune, Département, etc...)
        'Avant de lancer la commande, il faut voir quel case a été cochée, un doit les faire 1 par 1, c'est lourd mais bon (checkedlistbox ne marche pas ici pour un pb de visuel)
        Dim colInfos As New Collection
        Dim intNbrInfos As Integer = 0

        'Insertion de la ville
        If objForm.CheckBox_Ville.Checked = True Then
            Dim strCommune As String = objForm.ComboBox_Commune.Text.ToUpper.Remove(objForm.ComboBox_Commune.Text.Length - 8)
            Dim bolVoyelle As Boolean = StartsVoyelle(strCommune)
            If strCommune.ToUpper.Contains("PARIS") Then
                colInfos.Add("VILLE DE " & strCommune)
            ElseIf bolVoyelle = True Then
                colInfos.Add("COMMUNE D'" & strCommune)
            Else
                colInfos.Add("COMMUNE DE " & strCommune)
            End If
        End If
        'Insertion du département
        If objForm.CheckBox_Dpt.Checked = True Then
            colInfos.Add(objForm.Lbl_Departement.Text)
        End If
        'Insertion de l'adresse
        If objForm.CheckBox_Adresse.Checked = True Then
            colInfos.Add("Propriété sise / Immeuble sis / Terrain sis")
        End If
        'Insertion de la référence Cadastrale
        If objForm.CheckBox_Cad.Checked = True Then
            colInfos.Add("Cadastré(e) section xx numéro xx")
        End If
        'Insertion de la contenance
        If objForm.CheckBox_Contenance.Checked = True Then
            colInfos.Add("Pour une contenance cadastrale de xx hectares xx ares et xx centiares")
        End If
        'Insertion de la superficie
        If objForm.CheckBox_Sup.Checked = True Then
            colInfos.Add("Et une superficie réelle de xx m²")
        End If
        Inserer_Infos(colInfos)

        ed.WriteMessage("Fin de la fonction RVTitre")

    End Sub


    Public Sub InsererTitre(ByVal TypeTitre As String, ByVal strEchelle As String, ByVal strDossier As String, ByVal strDoc As String, ByVal strDate As String, ByVal strModif As String)

        'Saisie du point d'insertion par l'utilisateur
        Dim ppr As PromptPointResult = ed.GetPoint(vbLf & "Point d'insertion (en bas à droite): ")
        If ppr.Status <> PromptStatus.OK Then
            Return
        End If
        insPt = ppr.Value.TransformBy(ed.CurrentUserCoordinateSystem)
        'On insère le block
        Using tr As Transaction = db.TransactionManager.StartTransaction()
            Dim btr As BlockTableRecord = DirectCast(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
            Dim br As BlockReference = btr.InsertBlock(TypeTitre, insPt, New Scale3d(1.0), 0.0, ed)
            If br IsNot Nothing Then
                Dim atts As New Dictionary(Of String, String)()
                atts.Add("NOTA1", "")
                atts.Add("NOTA2", "")
                atts.Add("NOTA3", "")
                atts.Add("ECHELLE", "ECHELLE : " & strEchelle)
                atts.Add("DOSSIER", "DOSSIER : " & strDossier)
                atts.Add("DOCUMENT", "DOCUMENT : " & strDoc)
                atts.Add("DATE", "DATE : " & strDate)
                atts.Add("MODIFICATION", "MODIFICATION : " & strModif)
                br.AddAttributeReferences(atts)
            End If
            tr.Commit()
        End Using
    End Sub


    Public Sub Inserer_Block_TitredeBase(ByVal strCommune As String, ByVal strDept As String)

        Using tr As Transaction = db.TransactionManager.StartTransaction()
            Dim btr As BlockTableRecord = DirectCast(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
            Dim br As BlockReference = btr.InsertBlock("Entete (2 lignes)", insPt, New Scale3d(1.0), 0.0, ed)
            If br IsNot Nothing Then
                'Il faut gérer le cas en fonction du titre inséré (1 ligne ou 2 lignes)
                Dim atts As New Dictionary(Of String, String)()
                Dim bolVoyelle As Boolean = StartsVoyelle(strCommune)
                If strCommune.ToUpper.Contains("PARIS") Then
                    atts.Add("COMMUNE", "VILLE DE " & strCommune.Remove(strCommune.Length - 8))
                ElseIf bolVoyelle = True Then
                    atts.Add("COMMUNE", "COMMUNE D'" & strCommune.Remove(strCommune.Length - 8))
                Else
                    atts.Add("COMMUNE", "COMMUNE DE " & strCommune.Remove(strCommune.Length - 8))
                End If
                atts.Add("DEPT", strDept)
                br.AddAttributeReferences(atts)
            End If
                tr.Commit()
        End Using

    End Sub


    Public Sub Inserer_TypePlan(ByVal strTypeDessin As String, ByVal TypeBlockTexte As String)

        Using tr As Transaction = db.TransactionManager.StartTransaction()
            Dim btr As BlockTableRecord = DirectCast(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
            Dim br As BlockReference = btr.InsertBlock(TypeBlockTexte, insPt, New Scale3d(1.0), 0.0, ed)
            If br IsNot Nothing Then
                'Il faut gérer le cas en fonction du titre inséré (1 ligne ou 2 lignes)
                Dim atts As New Dictionary(Of String, String)()
                If TypeBlockTexte = "Titre (1 ligne)" Then
                    atts.Add("TITRE", strTypeDessin)
                    br.AddAttributeReferences(atts)
                Else
                    Dim strtTypeDessin As String() = strTypeDessin.Split("/")
                    atts.Add("TITRE1", strtTypeDessin(0).Trim)
                    atts.Add("TITRE2", strtTypeDessin(1).Trim)
                    br.AddAttributeReferences(atts)
                End If
            End If
            tr.Commit()
        End Using

    End Sub


    Sub Inserer_Logo(ByVal strLogo As String)

        Using tr As Transaction = db.TransactionManager.StartTransaction()
            Dim logopt As Point3d = New Point3d(insPt.X - 200, insPt.Y + 40, insPt.Z)
            Dim btr As BlockTableRecord = DirectCast(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
            Dim br As BlockReference = btr.InsertBlock(strLogo, logopt, New Scale3d(1.0), 0.0, ed)
            tr.Commit()
        End Using

    End Sub


    Sub Inserer_Descript(ByVal intNbrDescript As Integer)

        Dim toppt As Point3d = New Point3d(insPt.X - 97, insPt.Y + 130, insPt.Z)
        Application.SetSystemVariable("TEXTSTYLE", "LEGENDE")
        Using tr As Transaction = db.TransactionManager.StartTransaction()
            For i = 1 To intNbrDescript
                Dim btr As BlockTableRecord = DirectCast(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
                Dim acText As DBText = New DBText
                acText.SetDatabaseDefaults()
                acText.HorizontalMode = TextHorizontalMode.TextCenter
                acText.VerticalMode = TextVerticalMode.TextVerticalMid
                acText.TextString = "xxxxxx"
                acText.Height = 3.5
                acText.AlignmentPoint = toppt
                btr.AppendEntity(acText)
                tr.AddNewlyCreatedDBObject(acText, True)
                toppt = New Point3d(toppt.X, toppt.Y - 6, toppt.Z)
            Next
            tr.Commit()
        End Using

    End Sub


    Sub Inserer_Infos(ByVal colInfos As Collection)
        'On va remplacer les block utilisés par RVApplis par de simples textes
        'On change la variable system de style de texte
        Application.SetSystemVariable("TEXTSTYLE", "Titre")
        'On calcule la position du premier texte en fonction du nombre d'éléments à insérer
        Dim milpt As Point3d = New Point3d(insPt.X - 97, insPt.Y + 213.8, insPt.Z)
        Dim toppt As Point3d
        If colInfos.Count > 1 Then 'Sinon on multiplie par 0
            toppt = New Point3d(milpt.X, milpt.Y + (colInfos.Count - 1) * (7.7 / 2), milpt.Z)
        Else
            toppt = milpt
        End If
        'On insère les textes en bouclant sur le nombre de textes à insérer
        Using tr As Transaction = db.TransactionManager.StartTransaction()
            For Each strInfo As String In colInfos
                Dim btr As BlockTableRecord = DirectCast(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
                Dim acText As DBText = New DBText
                'On gère de le type de texte en fonction de ce qui est écrit
                If strInfo.ToUpper.Contains("VILLE") Or strInfo.ToUpper.Contains("COMMUNE") Or strInfo.ToUpper.Contains("PARTEMENT") Then
                    Application.SetSystemVariable("TEXTSTYLE", "Titre Gras")
                Else
                    Application.SetSystemVariable("TEXTSTYLE", "Titre")
                End If
                'On fini d'insérer le texte
                acText.SetDatabaseDefaults()
                acText.HorizontalMode = TextHorizontalMode.TextCenter
                acText.VerticalMode = TextVerticalMode.TextVerticalMid
                acText.TextString = strInfo
                acText.Height = 3.5
                acText.AlignmentPoint = toppt
                btr.AppendEntity(acText)
                tr.AddNewlyCreatedDBObject(acText, True)
                toppt = New Point3d(toppt.X, toppt.Y - 7.7, toppt.Z)
            Next
            tr.Commit()
        End Using


    End Sub

#End Region


#Region "Pliage"

    <CommandMethod("rvpliage")> Sub Pliage()

        'On requête les 3 points pour les marques
        Dim pt1 As Point3d = SelectPoint(ed, "Point bas gauche : ")
        If pt1.X = 777.777 Then
            Exit Sub
        End If
        Dim pt2 As Point3d = SelectPoint(ed, "Point bas droite : ")
        If pt2.X = 777.777 Then
            Exit Sub
        End If
        Dim pt3 As Point3d = SelectPoint(ed, "Point haut gauche : ")
        If pt3.X = 777.777 Then
            Exit Sub
        End If
        Dim pt4 As Point3d = New Point3d(pt2.X, pt3.Y, 0)

        'On vérifie que les axes sont orthonormés et perpendiculaires
        If pt1.Y <> pt2.Y Or pt1.X <> pt3.X Then
            ed.WriteMessage("Les axes sélectionnés ne sont pas orthonormés")
            Exit Sub
        End If

        'On regarde s'il s'agit bien de multiples de formats A4
        Dim distHz As Double = Math.Round(pt1.DistanceTo(pt2), 2)
        Dim distVt As Double = Math.Round(pt1.DistanceTo(pt3), 2)

        If distHz Mod 210 <> 0 Or distVt Mod 148.5 <> 0 Then '148.5 car le format vertical peut être coupé en 2
            ed.WriteMessage("Le format sélectionné n'est pas un multiple du A4")
            Exit Sub
        End If

        'On compte le nombre de formats A4
        Dim nbHz = distHz / 210
        Dim nbVt = distVt / 148.5

        'On ajoute une marque de pliage à l'intérieur si titre et hauteur supérieure à A4
        Dim TitreRes As String = ""
        If nbVt > 1 Then
            Dim pStrOpts As PromptStringOptions = New PromptStringOptions(vbLf & "Position du titre ? Aucun/Bas Gauche/HG/BD/HD : ")
            Dim PstrRes As PromptResult = ed.GetString(pStrOpts)
            TitreRes = PstrRes.StringResult
        End If

        'On dessine les marques de pliage
        Dim ptBaseBas As Point3d
        Dim ptBaseHaut As Point3d
        Dim ptBaseGauche As Point3d
        Dim ptBaseDroite As Point3d

        'Coin bas gauche
        DessinerLigne(db, pt1, New Point3d(pt1.X + 2.5, pt1.Y, 0))
        DessinerLigne(db, pt1, New Point3d(pt1.X, pt1.Y + 2.5, 0))

        'Coin haut gauche
        DessinerLigne(db, pt3, New Point3d(pt3.X + 2.5, pt3.Y, 0))
        DessinerLigne(db, pt3, New Point3d(pt3.X, pt3.Y - 2.5, 0))

        'Coin bas droite
        DessinerLigne(db, pt2, New Point3d(pt2.X - 2.5, pt2.Y, 0))
        DessinerLigne(db, pt2, New Point3d(pt2.X, pt2.Y + 2.5, 0))

        'Coin haut gauche
        DessinerLigne(db, pt4, New Point3d(pt4.X - 2.5, pt4.Y, 0))
        DessinerLigne(db, pt4, New Point3d(pt4.X, pt4.Y - 2.5, 0))

        If nbHz > 1 Then
            For i = 1 To nbHz - 1
                ptBaseBas = New Point3d(pt1.X + i * 210, pt1.Y, 0)
                DessinerLigne(db, ptBaseBas, New Point3d(ptBaseBas.X - 2.5, ptBaseBas.Y, 0))
                DessinerLigne(db, ptBaseBas, New Point3d(ptBaseBas.X + 2.5, ptBaseBas.Y, 0))
                DessinerLigne(db, ptBaseBas, New Point3d(ptBaseBas.X, ptBaseBas.Y + 2.5, 0))
                ptBaseHaut = New Point3d(pt3.X + i * 210, pt3.Y, 0)
                DessinerLigne(db, ptBaseHaut, New Point3d(ptBaseHaut.X - 2.5, ptBaseHaut.Y, 0))
                DessinerLigne(db, ptBaseHaut, New Point3d(ptBaseHaut.X + 2.5, ptBaseHaut.Y, 0))
                DessinerLigne(db, ptBaseHaut, New Point3d(ptBaseHaut.X, ptBaseHaut.Y - 2.5, 0))
            Next
        End If

        If nbVt > 2 Then
            For i = 2 To nbVt - 1
                If i Mod 2 = 0 Then 'On regarde si "i" est pair
                    ptBaseGauche = New Point3d(pt1.X, pt1.Y + i * 148.5, 0)
                    DessinerLigne(db, ptBaseGauche, New Point3d(ptBaseGauche.X, ptBaseGauche.Y - 2.5, 0))
                    DessinerLigne(db, ptBaseGauche, New Point3d(ptBaseGauche.X, ptBaseGauche.Y + 2.5, 0))
                    DessinerLigne(db, ptBaseGauche, New Point3d(ptBaseGauche.X + 2.5, ptBaseGauche.Y, 0))
                    ptBaseDroite = New Point3d(pt2.X, pt2.Y + i * 148.5, 0)
                    DessinerLigne(db, ptBaseDroite, New Point3d(ptBaseDroite.X, ptBaseDroite.Y - 2.5, 0))
                    DessinerLigne(db, ptBaseDroite, New Point3d(ptBaseDroite.X, ptBaseDroite.Y + 2.5, 0))
                    DessinerLigne(db, ptBaseDroite, New Point3d(ptBaseDroite.X - 2.5, ptBaseDroite.Y, 0))
                End If
            Next
        End If

        Select Case True
            Case TitreRes.ToUpper = "Aucun" Or TitreRes.ToUpper = "A"
            Case TitreRes.ToUpper = "BAS GAUCHE" Or TitreRes.ToUpper = "BG" Or TitreRes.ToUpper = "B G"
                DessinerLigne(db, New Point3d(pt1.X + 210, pt1.Y + 297, 0), New Point3d(pt1.X + 210 - 2.5, pt1.Y + 297, 0))
                DessinerLigne(db, New Point3d(pt1.X + 210, pt1.Y + 297 + 2.5, 0), New Point3d(pt1.X + 210, pt1.Y + 297 - 2.5, 0))
            Case TitreRes.ToUpper = "HAUT GAUCHE" Or TitreRes.ToUpper = "HG" Or TitreRes.ToUpper = "H G"
                DessinerLigne(db, New Point3d(pt3.X + 210 + 2.5, pt3.Y - 297, 0), New Point3d(pt3.X + 210 - 2.5, pt3.Y - 297, 0))
                DessinerLigne(db, New Point3d(pt3.X + 210, pt3.Y - 297 + 2.5, 0), New Point3d(pt3.X + 210, pt3.Y - 297 - 2.5, 0))
            Case TitreRes.ToUpper = "BAS DROITE" Or TitreRes.ToUpper = "BD" Or TitreRes.ToUpper = "B D"
                DessinerLigne(db, New Point3d(pt2.X - 210 + 2.5, pt3.Y + 297, 0), New Point3d(pt2.X - 210 - 2.5, pt3.Y + 297, 0))
                DessinerLigne(db, New Point3d(pt2.X - 210, pt2.Y + 297 + 2.5, 0), New Point3d(pt2.X - 210, pt2.Y + 297 - 2.5, 0))
            Case TitreRes.ToUpper = "HAUT DROITE" Or TitreRes.ToUpper = "HD" Or TitreRes.ToUpper = "H D"
                DessinerLigne(db, New Point3d(pt4.X - 210 + 2.5, pt4.Y - 297, 0), New Point3d(pt4.X - 210 - 2.5, pt4.Y - 297, 0))
                DessinerLigne(db, New Point3d(pt4.X - 210, pt4.Y - 297 + 2.5, 0), New Point3d(pt4.X - 210, pt4.Y - 297 - 2.5, 0))
        End Select

        ed.WriteMessage("Fin de la fonction RVPliage")

    End Sub

#End Region


#Region "OLD"

    'Sub InsererTitre(ByVal acDb As Database, ByVal acEd As Editor)

    '    Using trans As Transaction = acDb.TransactionManager.StartTransaction

    '        'On récupère le blocktablerecord
    '        Dim bt As BlockTable = trans.GetObject(acDb.BlockTableId, OpenMode.ForWrite)
    '        Dim btr As BlockTableRecord = bt(BlockTableRecord.ModelSpace).GetObject(OpenMode.ForWrite)

    '        'Création d'un base de données temporaire
    '        Dim acDbTemp As Database = New Database(False, False)
    '        acDbTemp.ReadDwgFile("D:\DEFACTO.dwg", IO.FileShare.Read, False, "")

    '        'On demande à l'utilisateur de saisier le point d'insertion
    '        Dim ppo As PromptPointOptions = New PromptPointOptions("Point en bas à droite :" & vbCrLf)
    '        Dim ppr As PromptPointResult = acEd.GetPoint(ppo)

    '        'On insère le bloc dans la database temporaire
    '        Dim strCheminBloc As String = "D:\DEFACTO.dwg"
    '        Dim objId As ObjectId = acDb.Insert(strCheminBloc, acDbTemp, True)

    '        'On réalise l'insertion dans le dessin courant
    '        Dim blockinsert As BlockReference = New BlockReference(ppr.Value, objId)
    '        btr.AppendEntity(blockinsert)
    '        trans.TransactionManager.AddNewlyCreatedDBObject(blockinsert, True)

    '        Dim prout As BlockReference = blockinsert

    '        'On va changer les valeurs des attributs
    '        Dim blkbtr As BlockTableRecord = trans.GetObject(blockinsert.BlockTableRecord, OpenMode.ForRead)
    '        Dim attEnt As Entity
    '        For Each attID As ObjectId In blkbtr
    '            attEnt = trans.GetObject(attID, OpenMode.ForRead)
    '            If TypeOf attEnt Is AttributeDefinition Then
    '                Dim AttDef As AttributeDefinition = attEnt
    '                Dim attRef As AttributeReference = New AttributeReference()
    '                attRef.SetAttributeFromBlock(AttDef, blockinsert.BlockTransform)
    '                blockinsert.AttributeCollection.AppendAttribute(attRef)
    '                attRef.TextString = "Test"
    '                trans.TransactionManager.AddNewlyCreatedDBObject(attRef, True)
    '            End If
    '            Dim attRef As AttributeReference
    '            attRef = trans.GetObject(attID, OpenMode.ForRead)
    '            MsgBox(attRef.Tag)

    '        Next

    '        'On valide et réalise les changements
    '        trans.Commit()
    '        acDb.Dispose()
    '        acDbTemp.Dispose()

    '    End Using

    'End Sub

#End Region

End Class
