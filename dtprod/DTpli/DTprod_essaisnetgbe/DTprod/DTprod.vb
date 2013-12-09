Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.EditorInput
Imports Autodesk.AutoCAD.Runtime
Imports System.IO
Imports Autodesk.AutoCAD.Geometry

Public Class DTprod

    Dim insPt As Point3d
    Dim doc As Document = Application.DocumentManager.MdiActiveDocument
    Dim db As Database = doc.Database
    Dim ed As Editor = doc.Editor

#Region "Pliage"

    <CommandMethod("DTpli")> Sub Pliage()

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

        ed.WriteMessage("Fin de la fonction DTpli")

    End Sub

#End Region

End Class
