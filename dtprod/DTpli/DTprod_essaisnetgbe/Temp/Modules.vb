Imports Autodesk.AutoCAD.DatabaseServices
Imports System.IO
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.EditorInput
Imports Autodesk.AutoCAD.ApplicationServices

Module BlockExtension
    ' Insère une référence de bloc
    <System.Runtime.CompilerServices.Extension()> _
    Public Function InsertBlock(ByVal btr As BlockTableRecord, ByVal blockName As String, ByVal insertPoint As Point3d, ByVal scale As Scale3d, ByVal angle As Double, ByVal ed As Editor) As BlockReference

        Dim db As Database = btr.Database
        Dim tr As Transaction = db.TransactionManager.TopTransaction
        Dim bt As BlockTable = TryCast(tr.GetObject(db.BlockTableId, OpenMode.ForRead), BlockTable)
        ' Récupérer l'ObjectId du bloc 'blockName' (importé s'il n'était pas présent dans la table des blocs)
        Dim btrId As ObjectId = GetBlock(bt, blockName)
        ' si le bloc n' a pu être importé, retour = ObjectId.Null
        If btrId = ObjectId.Null Then
            ed.WriteMessage(vbCrLf & "Impossible d'insérer le bloc...Traitement annulé")
            Return (Nothing)
        End If
        ' Insertion du bloc au point d'insertion avec l'échelle, la rotation et le calque spécifié en fonction du SCU courant
        Dim br As New BlockReference(insertPoint, btrId) With {.ScaleFactors = scale, .Rotation = angle}
        br.RecordGraphicsModified(True)
        ' Ajout du bloc à l'espace courant
        Dim space As BlockTableRecord = DirectCast(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
        space.AppendEntity(br)
        tr.AddNewlyCreatedDBObject(br, True)
        Return br

    End Function

    ' Obtient l'ObjectId d'une définition de bloc d'après son chemin d'accès
    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetBlock(ByVal blockTable As BlockTable, ByVal blockName As String) As ObjectId

        Dim db As Database = blockTable.Database
        Try
            Using tmpDb As New Database(False, True)
                Dim blockPath As String = "O:\Bibliothèque des symboles\Titres\" & blockName & ".dwg"
                tmpDb.ReadDwgFile(blockPath, FileShare.Read, True, Nothing)
                Return blockTable.Database.Insert(blockName, tmpDb, True)
            End Using
            ' si le fichier n'est pas trouvé, retourne ObjectId.Null
        Catch
            Return ObjectId.Null
        End Try

    End Function

    ' Ajoute les références d'attributs à la référence de bloc et leur affecte leurs valeurs.
    <System.Runtime.CompilerServices.Extension()> _
    Public Sub AddAttributeReferences(ByVal br As BlockReference, ByVal attValues As Dictionary(Of String, String))
        ' définition de la référence de bloc
        Dim tr As Transaction = br.Database.TransactionManager.TopTransaction
        Dim btr As BlockTableRecord = DirectCast(tr.GetObject(br.BlockTableRecord, OpenMode.ForRead), BlockTableRecord)
        For Each id As ObjectId In btr
            If id.ObjectClass.Name = "AcDbAttributeDefinition" Then
                ' copier la géométrie sur la définition d'attribut
                Dim attDef As AttributeDefinition = DirectCast(tr.GetObject(id, OpenMode.ForRead), AttributeDefinition)
                Dim attRef As New AttributeReference()
                attRef.SetAttributeFromBlock(attDef, br.BlockTransform)
                ' affecter la valeur en fonction du paramètre 'attValues'
                If attValues.ContainsKey(attDef.Tag.ToUpper()) Then
                    attRef.TextString = attValues(attDef.Tag.ToUpper())
                End If
                br.AttributeCollection.AppendAttribute(attRef)
                tr.AddNewlyCreatedDBObject(attRef, True)
            End If
        Next
    End Sub

    'Check si la première lettre d'une string est une voyelle
    <System.Runtime.CompilerServices.Extension()> _
    Public Function StartsVoyelle(ByVal testTexte As String) As Boolean

        Dim isVoyelle As Boolean = False
        Select Case True
            Case testTexte.ToUpper.StartsWith("A")
                isVoyelle = True
            Case testTexte.ToUpper.StartsWith("E")
                isVoyelle = True
            Case testTexte.ToUpper.StartsWith("I")
                isVoyelle = True
            Case testTexte.ToUpper.StartsWith("O")
                isVoyelle = True
            Case testTexte.ToUpper.StartsWith("U")
                isVoyelle = True
            Case testTexte.ToUpper.StartsWith("Y")
                isVoyelle = True
        End Select

        Return isVoyelle

    End Function

    'Requête d'un point à l'utilisateur
    <System.Runtime.CompilerServices.Extension()> _
    Public Function SelectPoint(ByVal ed As Editor, ByVal strMessage As String) As Point3d

        Dim ptRes As PromptPointResult
        Dim ptOpts As PromptPointOptions = New PromptPointOptions("")
        ptOpts.Message = vbCrLf & strMessage
        ptRes = ed.GetPoint(ptOpts)
        Dim SelectedPt As Point3d = ptRes.Value

        If ptRes.Status <> PromptStatus.OK Then
            SelectedPt = New Point3d(777.777, 0, 0)
        End If

        Return SelectedPt

    End Function

    'Dessin d'une ligne
    <System.Runtime.CompilerServices.Extension()> _
    Public Sub DessinerLigne(ByVal db As Database, ByVal ptA As Point3d, ByVal ptB As Point3d)

        Using tr As Transaction = db.TransactionManager.StartTransaction
            Dim bt As BlockTable = tr.GetObject(db.BlockTableId, OpenMode.ForRead)
            Dim btr As BlockTableRecord = tr.GetObject(bt(BlockTableRecord.PaperSpace), OpenMode.ForWrite)

            Dim acLine As Line = New Line(ptA, ptB)
            acLine.SetDatabaseDefaults()

            btr.AppendEntity(acLine)
            tr.AddNewlyCreatedDBObject(acLine, True)
            tr.Commit()

        End Using

    End Sub



#Region "OLD"

    '' Obtient l'ObjectId d'une définition de bloc d'après son nom. 
    '' Si le bloc n'est pas trouvé dans la collection, un fichier dwg est recherché dans les chemins de support 
    '' et importé dans la table des blocs.
    '<System.Runtime.CompilerServices.Extension()> _
    'Public Function GetBlock(ByVal blockTable As BlockTable, ByVal blockName As String) As ObjectId

    '    Dim db As Database = blockTable.Database
    '    ' si la table contient déjà le bloc, son ObjectId est retourné
    '    If blockTable.Has(blockName) Then
    '        Return blockTable(blockName)
    '    End If
    '    ' recherche d'un fichier du nom du bloc dans les chemins de recherche
    '    Try
    '        Dim blockPath As String = HostApplicationServices.Current.FindFile(blockName & ".dwg", db, FindFileHint.[Default])
    '        ' importe le fichier dans la table des blocs et retourne son ObjectId
    '        blockTable.UpgradeOpen()
    '        Using tmpDb As New Database(False, True)
    '            tmpDb.ReadDwgFile(blockPath, FileShare.Read, True, Nothing)
    '            Return blockTable.Database.Insert(blockName, tmpDb, True)
    '        End Using
    '        ' si le fichier n'est pas trouvé, retourne ObjectId.Null
    '    Catch
    '        Return ObjectId.Null
    '    End Try

    'End Function

#End Region

End Module


