package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"os"
	"path/filepath"
	"strings"

	"golang.org/x/tools/go/packages"
)

// ProjectTypeInfo contains type information for the entire project
type ProjectTypeInfo struct {
	FileSet  *token.FileSet
	Packages map[string]*PackageInfo
}

// PackageInfo contains type information for a single package
type PackageInfo struct {
	Package *types.Package
	Info    *types.Info
	Files   []*ast.File
	Syntax  []*ast.File
}

// BuildProjectTypeInfo builds type information for the entire project
func BuildProjectTypeInfo(projectPath string) (*ProjectTypeInfo, error) {
	return BuildProjectTypeInfoWithMode(projectPath, packages.NeedName|packages.NeedFiles|packages.NeedCompiledGoFiles|packages.NeedImports|packages.NeedTypes|packages.NeedSyntax|packages.NeedTypesInfo)
}

// BuildProjectTypeInfoWithMode builds type information with specified load mode
func BuildProjectTypeInfoWithMode(projectPath string, mode packages.LoadMode) (*ProjectTypeInfo, error) {
	// Configuration for loading packages
	cfg := &packages.Config{
		Mode: mode,
		Dir:  projectPath,
		Fset: token.NewFileSet(),
	}

	// Load all project packages
	pkgs, err := packages.Load(cfg, "./...")
	if err != nil {
		return nil, fmt.Errorf("failed to load packages: %w", err)
	}

	// Check for loading errors
	if packages.PrintErrors(pkgs) > 0 {
		return nil, fmt.Errorf("packages contain errors")
	}

	// Create structure for storing information
	projectInfo := &ProjectTypeInfo{
		FileSet:  cfg.Fset,
		Packages: make(map[string]*PackageInfo),
	}

	// Process each package
	for _, pkg := range pkgs {
		if pkg.Types == nil {
			continue
		}

		packageInfo := &PackageInfo{
			Package: pkg.Types,
			Info:    pkg.TypesInfo,
			Files:   pkg.Syntax,
			Syntax:  pkg.Syntax,
		}

		projectInfo.Packages[pkg.PkgPath] = packageInfo
	}

	return projectInfo, nil
}

// GetPackageInfo returns package information by path
func (pti *ProjectTypeInfo) GetPackageInfo(pkgPath string) *PackageInfo {
	return pti.Packages[pkgPath]
}

// GetTypeInfo returns types.Info for a package
func (pti *ProjectTypeInfo) GetTypeInfo(pkgPath string) *types.Info {
	if pkg := pti.Packages[pkgPath]; pkg != nil {
		return pkg.Info
	}
	return nil
}

// GetPackage returns types.Package for a package
func (pti *ProjectTypeInfo) GetPackage(pkgPath string) *types.Package {
	if pkg := pti.Packages[pkgPath]; pkg != nil {
		return pkg.Package
	}
	return nil
}

// FindPackageByFile finds a package by file
func (pti *ProjectTypeInfo) FindPackageByFile(filename string) *PackageInfo {
	absPath, err := filepath.Abs(filename)
	if err != nil {
		return nil
	}

	for _, pkgInfo := range pti.Packages {
		for _, file := range pkgInfo.Files {
			pos := pti.FileSet.Position(file.Pos())
			if filepath.Clean(pos.Filename) == filepath.Clean(absPath) {
				return pkgInfo
			}
		}
	}
	return nil
}

// CreateEnhancedTypeResolver creates an enhanced type resolver for the project
func CreateEnhancedTypeResolver(projectInfo *ProjectTypeInfo) *EnhancedTypeResolver {
	return &EnhancedTypeResolver{
		ProjectInfo: projectInfo,
	}
}

// EnhancedTypeResolver - enhanced type resolver with full project support
type EnhancedTypeResolver struct {
	ProjectInfo *ProjectTypeInfo
}

// TypeStructure represents the complete structure of a type
type TypeStructure struct {
	Name        string            `json:"name"`
	Kind        string            `json:"kind"` // "struct", "slice", "map", "basic", etc.
	Package     string            `json:"package,omitempty"`
	Comment     string            `json:"comment,omitempty"`
	Fields      []FieldInfo       `json:"fields,omitempty"`
	ElementType *TypeStructure    `json:"element_type,omitempty"` // for slice, array
	KeyType     *TypeStructure    `json:"key_type,omitempty"`     // for map
	ValueType   *TypeStructure    `json:"value_type,omitempty"`   // for map
	Methods     []MethodInfo      `json:"methods,omitempty"`
	Embedded    []TypeStructure   `json:"embedded,omitempty"`
	Tags        map[string]string `json:"tags,omitempty"`
}

// FieldInfo represents information about a struct field
type FieldInfo struct {
	Name     string            `json:"name"`
	Type     *TypeStructure    `json:"type"`
	Tag      string            `json:"tag,omitempty"`
	Comment  string            `json:"comment,omitempty"`
	Embedded bool              `json:"embedded,omitempty"`
	Exported bool              `json:"exported"`
	Tags     map[string]string `json:"tags,omitempty"`
}

// MethodInfo represents information about a method
type MethodInfo struct {
	Name       string          `json:"name"`
	Signature  string          `json:"signature"`
	Comment    string          `json:"comment,omitempty"`
	Exported   bool            `json:"exported"`
	Receiver   *TypeStructure  `json:"receiver,omitempty"`
	Parameters []ParameterInfo `json:"parameters,omitempty"`
	Results    []ParameterInfo `json:"results,omitempty"`
}

// ParameterInfo represents information about a function parameter
type ParameterInfo struct {
	Name string         `json:"name,omitempty"`
	Type *TypeStructure `json:"type"`
}

// GetArgumentType determines the type of an argument with full project context
func (etr *EnhancedTypeResolver) GetArgumentType(arg ast.Expr, currentFile string) types.Type {
	// Find package for current file
	//pkgInfo := etr.ProjectInfo.FindPackageByFile(currentFile)
	pkgInfo := etr.ProjectInfo.Packages[currentFile]
	if pkgInfo == nil {
		return nil
	}

	// Use type information from the package
	if typ := pkgInfo.Info.TypeOf(arg); typ != nil {
		return typ
	}

	// Fallback to basic AST analysis
	return etr.fallbackTypeResolution(arg, pkgInfo)
}

// GetArgumentTypeStructure returns the complete structure of an argument type
func (etr *EnhancedTypeResolver) GetArgumentTypeStructure(arg ast.Expr, currentFile string) *TypeStructure {
	// Find package for current file
	pkgInfo := etr.ProjectInfo.Packages[currentFile]
	//pkgInfo := etr.ProjectInfo.FindPackageByFile(currentFile)
	if pkgInfo == nil {
		return nil
	}

	// Get argument type
	var typ types.Type
	if pkgInfo.Info != nil {
		typ = pkgInfo.Info.TypeOf(arg)
	}

	if typ == nil {
		return nil
	}

	// Create cache to avoid circular references
	cache := make(map[string]*TypeStructure)

	// Build complete type structure
	return etr.buildTypeStructure(typ, pkgInfo.Package, cache)
}

// buildTypeStructure recursively builds type structure
func (etr *EnhancedTypeResolver) buildTypeStructure(typ types.Type, currentPkg *types.Package, cache map[string]*TypeStructure) *TypeStructure {
	// Get string representation of type for cache
	typeKey := typ.String()
	if cached, exists := cache[typeKey]; exists {
		return cached
	}

	structure := &TypeStructure{
		Tags: make(map[string]string),
	}

	// Add to cache immediately to avoid circular references
	cache[typeKey] = structure

	switch t := typ.(type) {
	case *types.Named:
		structure.Name = t.Obj().Name()
		structure.Kind = "named"

		if t.Obj().Pkg() != nil {
			structure.Package = t.Obj().Pkg().Path()
		}

		// Get type comment
		structure.Comment = etr.getTypeComment(t)

		// Get methods
		structure.Methods = etr.getTypeMethods(t, currentPkg, cache)

		// Process underlying type
		underlying := etr.buildTypeStructure(t.Underlying(), currentPkg, cache)
		if underlying != nil {
			//origName := structure.Name
			//structure = underlying
			//structure.Name = origName
			if underlying.Kind != "struct" {
				structure.Package = underlying.Package
			}
			structure.Kind = underlying.Kind
			structure.Comment = underlying.Comment
			structure.Fields = underlying.Fields
			structure.ElementType = underlying.ElementType
			structure.KeyType = underlying.KeyType
			structure.ValueType = underlying.ValueType
			structure.Methods = underlying.Methods
			structure.Embedded = underlying.Embedded
			structure.Tags = underlying.Tags
			//if underlying.Kind == "struct" {
			//	structure.Kind = "struct"
			//}
			if underlying.Kind == "basic" {
				//structure.Kind = "basic"
				structure.Name = underlying.Name
			}
			//if structure.Comment == "" && underlying.Comment != "" {
			//	structure.Comment = underlying.Comment
			//}
		}

	case *types.Struct:
		structure.Kind = "struct"
		structure.Fields = etr.getStructFields(t, currentPkg, cache)

	case *types.Slice:
		structure.Kind = "slice"
		structure.ElementType = etr.buildTypeStructure(t.Elem(), currentPkg, cache)

	case *types.Array:
		structure.Kind = "array"
		structure.ElementType = etr.buildTypeStructure(t.Elem(), currentPkg, cache)

	case *types.Map:
		structure.Kind = "map"
		structure.KeyType = etr.buildTypeStructure(t.Key(), currentPkg, cache)
		structure.ValueType = etr.buildTypeStructure(t.Elem(), currentPkg, cache)

	case *types.Pointer:
		structure.Kind = "pointer"
		structure.ElementType = etr.buildTypeStructure(t.Elem(), currentPkg, cache)

	case *types.Interface:
		structure.Kind = "interface"
		structure.Methods = etr.getInterfaceMethods(t, currentPkg, cache)

	case *types.Chan:
		structure.Kind = "channel"
		structure.ElementType = etr.buildTypeStructure(t.Elem(), currentPkg, cache)

	case *types.Signature:
		structure.Kind = "function"
		structure.Methods = []MethodInfo{etr.getSignatureInfo(t, currentPkg, cache)}

	case *types.Basic:
		structure.Kind = "basic"
		structure.Name = t.Name()

	default:
		structure.Kind = "unknown"
		structure.Name = typ.String()
	}

	return structure
}

// getStructFields gets struct fields
func (etr *EnhancedTypeResolver) getStructFields(structType *types.Struct, currentPkg *types.Package, cache map[string]*TypeStructure) []FieldInfo {
	var fields []FieldInfo

	for i := 0; i < structType.NumFields(); i++ {
		field := structType.Field(i)
		tag := structType.Tag(i)

		fieldInfo := FieldInfo{
			Name:     field.Name(),
			Type:     etr.buildTypeStructure(field.Type(), currentPkg, cache),
			Tag:      tag,
			Embedded: field.Embedded(),
			Exported: field.Exported(),
			Tags:     etr.parseStructTag(tag),
		}

		// Get field comment
		fieldInfo.Comment = etr.getFieldComment(field)

		fields = append(fields, fieldInfo)
	}

	return fields
}

// getTypeMethods gets type methods
func (etr *EnhancedTypeResolver) getTypeMethods(namedType *types.Named, currentPkg *types.Package, cache map[string]*TypeStructure) []MethodInfo {
	var methods []MethodInfo

	for i := 0; i < namedType.NumMethods(); i++ {
		method := namedType.Method(i)

		methodInfo := MethodInfo{
			Name:      method.Name(),
			Exported:  method.Exported(),
			Signature: method.Type().String(),
		}

		// Get method comment
		methodInfo.Comment = etr.getMethodComment(method)

		// Parse method signature
		if sig, ok := method.Type().(*types.Signature); ok {
			methodInfo.Parameters = etr.getSignatureParams(sig.Params(), currentPkg, cache)
			methodInfo.Results = etr.getSignatureParams(sig.Results(), currentPkg, cache)
		}

		methods = append(methods, methodInfo)
	}

	return methods
}

// getInterfaceMethods gets interface methods
func (etr *EnhancedTypeResolver) getInterfaceMethods(interfaceType *types.Interface, currentPkg *types.Package, cache map[string]*TypeStructure) []MethodInfo {
	var methods []MethodInfo

	for i := 0; i < interfaceType.NumMethods(); i++ {
		method := interfaceType.Method(i)

		methodInfo := MethodInfo{
			Name:      method.Name(),
			Exported:  method.Exported(),
			Signature: method.Type().String(),
		}

		// Parse method signature
		if sig, ok := method.Type().(*types.Signature); ok {
			methodInfo.Parameters = etr.getSignatureParams(sig.Params(), currentPkg, cache)
			methodInfo.Results = etr.getSignatureParams(sig.Results(), currentPkg, cache)
		}

		methods = append(methods, methodInfo)
	}

	return methods
}

// getSignatureInfo gets function signature information
func (etr *EnhancedTypeResolver) getSignatureInfo(sig *types.Signature, currentPkg *types.Package, cache map[string]*TypeStructure) MethodInfo {
	return MethodInfo{
		Name:       "function",
		Signature:  sig.String(),
		Parameters: etr.getSignatureParams(sig.Params(), currentPkg, cache),
		Results:    etr.getSignatureParams(sig.Results(), currentPkg, cache),
	}
}

// getSignatureParams gets signature parameters
func (etr *EnhancedTypeResolver) getSignatureParams(params *types.Tuple, currentPkg *types.Package, cache map[string]*TypeStructure) []ParameterInfo {
	var paramInfos []ParameterInfo

	for i := 0; i < params.Len(); i++ {
		param := params.At(i)
		paramInfo := ParameterInfo{
			Name: param.Name(),
			Type: etr.buildTypeStructure(param.Type(), currentPkg, cache),
		}
		paramInfos = append(paramInfos, paramInfo)
	}

	return paramInfos
}

// parseStructTag parses struct tags
func (etr *EnhancedTypeResolver) parseStructTag(tag string) map[string]string {
	tags := make(map[string]string)

	if tag == "" {
		return tags
	}

	// Simple tag parser (can be replaced with more complex one)
	parts := strings.Split(tag, " ")
	for _, part := range parts {
		part = strings.Trim(part, "`")
		if colonIndex := strings.Index(part, ":"); colonIndex != -1 {
			key := part[:colonIndex]
			value := strings.Trim(part[colonIndex+1:], `"`)
			tags[key] = value
		}
	}

	return tags
}

// getTypeComment gets type comment
func (etr *EnhancedTypeResolver) getTypeComment(namedType *types.Named) string {
	// Search for comment in AST
	obj := namedType.Obj()
	if obj.Pkg() == nil {
		return ""
	}

	pkgInfo := etr.ProjectInfo.GetPackageInfo(obj.Pkg().Path())
	if pkgInfo == nil {
		return ""
	}

	// Search for type declaration in AST
	for _, file := range pkgInfo.Files {
		for _, decl := range file.Decls {
			if genDecl, ok := decl.(*ast.GenDecl); ok {
				for _, spec := range genDecl.Specs {
					if typeSpec, ok := spec.(*ast.TypeSpec); ok {
						if typeSpec.Name.Name == obj.Name() {
							if genDecl.Doc != nil {
								return genDecl.Doc.Text()
							}
							return ""
						}
					}
				}
			}
		}
	}

	return ""
}

// getFieldComment gets field comment
func (etr *EnhancedTypeResolver) getFieldComment(field *types.Var) string {
	// Implementation of field comment search in AST
	// This is a more complex task requiring position matching in AST
	return ""
}

// getMethodComment gets method comment
func (etr *EnhancedTypeResolver) getMethodComment(method *types.Func) string {
	// Implementation of method comment search in AST
	return ""
}

// FormatType formats type for output
func (etr *EnhancedTypeResolver) FormatType(typ types.Type, currentPkg *types.Package) string {
	// Remove pointers to get base type
	if ptr, ok := typ.(*types.Pointer); ok {
		typ = ptr.Elem()
	}

	// If it's a named type, return its name
	if named, ok := typ.(*types.Named); ok {
		obj := named.Obj()
		if obj.Pkg() != nil && obj.Pkg() != currentPkg {
			// If type is from another package, include package name
			return obj.Pkg().Name() + "." + obj.Name()
		}
		return obj.Name()
	}

	// For other types, return their string representation
	return typ.String()
}

// fallbackTypeResolution - fallback method for type resolution
func (etr *EnhancedTypeResolver) fallbackTypeResolution(arg ast.Expr, pkgInfo *PackageInfo) types.Type {
	// Here you can add additional AST analysis logic
	// using project information
	return nil
}

// PrintTypeStructure prints type structure in readable format
func (etr *EnhancedTypeResolver) PrintTypeStructure(structure *TypeStructure, indent int) string {
	if structure == nil {
		return ""
	}

	indentStr := strings.Repeat("  ", indent)
	var result strings.Builder

	if structure.Kind == "pointer" && structure.ElementType != nil {
		result.WriteString(etr.PrintTypeStructure(structure.ElementType, indent))
	}

	// Print struct fields
	if structure.Kind == "struct" {
		// Print basic type information
		result.WriteString(fmt.Sprintf("%stype %s %s {\n", indentStr, etr.getTypeDisplayName(structure), structure.Kind))
		if structure.Package != "" {
			//result.WriteString(fmt.Sprintf(" (package: %s)", structure.Package))
		}
		//result.WriteString(fmt.Sprintf("{%s\n", indentStr))
		for _, field := range structure.Fields {
			if field.Embedded {
				result.WriteString(fmt.Sprintf("%s  %s", indentStr, etr.getTypeDisplayName(field.Type)))
			} else {
				result.WriteString(fmt.Sprintf("%s  %s %s", indentStr, field.Name, etr.getTypeDisplayName(field.Type)))
			}

			if field.Tag != "" {
				result.WriteString(fmt.Sprintf(" `%s`", field.Tag))
			}

			if field.Comment != "" {
				result.WriteString(fmt.Sprintf(" // %s", strings.TrimSpace(field.Comment)))
			}
			result.WriteString("\n")
		}
		result.WriteString(fmt.Sprintf("%s}\n", indentStr))

		// Print nested types
		for _, field := range structure.Fields {
			if field.Type.Kind == "struct" || field.Type.Kind == "named" {
				result.WriteString("\n")
				result.WriteString(etr.PrintTypeStructure(field.Type, indent))
			} else if field.Type.Kind == "pointer" {
				if field.Type.ElementType.Kind == "struct" {
					result.WriteString("\n")
					result.WriteString(etr.PrintTypeStructure(field.Type.ElementType, indent))
				}
				if (field.Type.ElementType.Kind == "slice" || field.Type.ElementType.Kind == "array") && field.Type.ElementType.ElementType.Kind == "struct" {
					result.WriteString("\n")
					result.WriteString(etr.PrintTypeStructure(field.Type.ElementType.ElementType, indent))
				}
				if field.Type.ElementType.Kind == "map" {
					if field.Type.ElementType.KeyType.Kind == "struct" {
						result.WriteString("\n")
						result.WriteString(etr.PrintTypeStructure(field.Type.ElementType.KeyType, indent))
					}
					if field.Type.ElementType.ValueType.Kind == "struct" {
						result.WriteString("\n")
						result.WriteString(etr.PrintTypeStructure(field.Type.ElementType.ValueType, indent))
					}
				}
			} else if (field.Type.Kind == "slice" || field.Type.Kind == "array") && field.Type.ElementType.Kind == "struct" {
				result.WriteString("\n")
				result.WriteString(etr.PrintTypeStructure(field.Type.ElementType, indent))
			} else if field.Type.Kind == "map" {
				if field.Type.KeyType.Kind == "struct" {
					result.WriteString("\n")
					result.WriteString(etr.PrintTypeStructure(field.Type.KeyType, indent))
				}
				if field.Type.ValueType.Kind == "struct" {
					result.WriteString("\n")
					result.WriteString(etr.PrintTypeStructure(field.Type.ValueType, indent))
				}
			}
		}
	}

	// Print slice/array information
	if (structure.Kind == "slice" || structure.Kind == "array") && structure.ElementType != nil {
		result.WriteString(fmt.Sprintf("%s%s\n", indentStr, etr.getTypeDisplayName(structure)))
		if structure.ElementType.Kind == "struct" {
			result.WriteString(etr.PrintTypeStructure(structure.ElementType, indent))
		}
		//result.WriteString(fmt.Sprintf("\n"))
	}

	// Print map information
	if structure.Kind == "map" {
		result.WriteString(fmt.Sprintf("%s%s\n", indentStr, etr.getTypeDisplayName(structure)))
		if structure.KeyType.Kind == "struct" {
			result.WriteString(etr.PrintTypeStructure(structure.KeyType, indent))
		}
		if structure.ValueType.Kind == "struct" {
			result.WriteString(etr.PrintTypeStructure(structure.ValueType, indent))
		}
	}
	if structure.Kind == "basic" {
		result.WriteString(fmt.Sprintf("%s%s\n", indentStr, etr.getTypeDisplayName(structure)))
	}
	if structure.Kind == "interface" {
		result.WriteString(fmt.Sprintf("%s%s\n", indentStr, etr.getTypeDisplayName(structure)))
	}

	// Print basic type information
	//result.WriteString(fmt.Sprintf("%s%s", indentStr, structure.Name))
	if structure.Package != "" {
		//result.WriteString(fmt.Sprintf(" (package: %s)", structure.Package))
	}

	if structure.Comment != "" {
		result.WriteString(fmt.Sprintf(" // %s", strings.TrimSpace(structure.Comment)))
	}

	return result.String()
}

// getTypeDisplayName gets the display name of a type
func (etr *EnhancedTypeResolver) getTypeDisplayName(structure *TypeStructure) string {
	if structure == nil {
		return "unknown"
	}

	switch structure.Kind {
	case "slice":
		return "[]" + etr.getTypeDisplayName(structure.ElementType)
	case "array":
		return "[]" + etr.getTypeDisplayName(structure.ElementType) // Simplified
	case "map":
		return fmt.Sprintf("map[%s]%s",
			etr.getTypeDisplayName(structure.KeyType),
			etr.getTypeDisplayName(structure.ValueType))
	case "pointer":
		return "*" + etr.getTypeDisplayName(structure.ElementType)
	case "channel":
		return "chan " + etr.getTypeDisplayName(structure.ElementType)
	case "interface":
		if structure.Name != "" {
			return structure.Name
		} else {
			return "interface{}"
		}
	default:
		if structure.Package != "" && structure.Name != "" {
			return structure.Package + "." + structure.Name
		}
		return structure.Name
	}
}

// getFieldCommentFromAST gets field comment from AST
func (etr *EnhancedTypeResolver) getFieldCommentFromAST(field *types.Var, structType *types.Named) string {
	if structType == nil || structType.Obj().Pkg() == nil {
		return ""
	}

	pkgInfo := etr.ProjectInfo.GetPackageInfo(structType.Obj().Pkg().Path())
	if pkgInfo == nil {
		return ""
	}

	// Search for struct declaration in AST
	for _, file := range pkgInfo.Files {
		for _, decl := range file.Decls {
			if genDecl, ok := decl.(*ast.GenDecl); ok {
				for _, spec := range genDecl.Specs {
					if typeSpec, ok := spec.(*ast.TypeSpec); ok {
						if typeSpec.Name.Name == structType.Obj().Name() {
							if structAST, ok := typeSpec.Type.(*ast.StructType); ok {
								// Search for field in struct
								for _, fieldAST := range structAST.Fields.List {
									for _, name := range fieldAST.Names {
										if name.Name == field.Name() {
											if fieldAST.Comment != nil {
												return fieldAST.Comment.Text()
											}
											if fieldAST.Doc != nil {
												return fieldAST.Doc.Text()
											}
											return ""
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	return ""
}

// GetArgumentTypeStructureWithComments returns complete type structure with comments
func (etr *EnhancedTypeResolver) GetArgumentTypeStructureWithComments(typ types.Type, currentFile string) *TypeStructure {
	// Find package for current file
	pkgInfo := etr.ProjectInfo.Packages[currentFile]
	//pkgInfo := etr.ProjectInfo.FindPackageByFile(currentFile)
	if pkgInfo == nil {
		return nil
	}

	// Create cache to avoid circular references
	cache := make(map[string]*TypeStructure)

	// Build complete type structure with comments
	return etr.buildTypeStructureWithComments(typ, pkgInfo.Package, cache)
}

// buildTypeStructureWithComments builds type structure with comments
func (etr *EnhancedTypeResolver) buildTypeStructureWithComments(typ types.Type, currentPkg *types.Package, cache map[string]*TypeStructure) *TypeStructure {
	structure := etr.buildTypeStructure(typ, currentPkg, cache)

	// Additionally process comments for structs
	if structure.Kind == "struct" || structure.Kind == "named" {
		if namedType, ok := typ.(*types.Named); ok {
			// Update field comments
			for i := range structure.Fields {
				if structType, ok := namedType.Underlying().(*types.Struct); ok {
					if i < structType.NumFields() {
						field := structType.Field(i)
						structure.Fields[i].Comment = etr.getFieldCommentFromAST(field, namedType)
					}
				}
			}
		}
	}

	return structure
}

// Example usage
func ExampleUsage() {
	// Build type information for the entire project
	projectInfo, err := BuildProjectTypeInfo(".")
	if err != nil {
		log.Fatal(err)
	}

	// Create enhanced resolver
	//resolver := CreateEnhancedTypeResolver(projectInfo)

	// Print information about found packages
	fmt.Printf("Found %d packages:\n", len(projectInfo.Packages))
	for pkgPath, pkgInfo := range projectInfo.Packages {
		fmt.Printf("- %s (files: %d)\n", pkgPath, len(pkgInfo.Files))
	}

	// Example usage for getting type structure
	/*
		// For CallExpr with argument:
		if callExpr, ok := node.(*ast.CallExpr); ok {
			if len(callExpr.Args) > 0 {
				// Get simple type name
				argType := resolver.GetArgumentType(callExpr.Args[0], "current/file.go")
				fmt.Printf("Argument type: %s\n", argType)

				// Get complete type structure
				structure := resolver.GetArgumentTypeStructureWithComments(callExpr.Args[0], "current/file.go")
				if structure != nil {
					fmt.Printf("Full type structure:\n")
					fmt.Print(resolver.PrintTypeStructure(structure, 0))
				}
			}
		}
	*/
}

// ExampleStructureOutput demonstrates type structure output
func ExampleStructureOutput() {
	// Example of how the output will look for a structure:
	/*
		type getFirstMessageResponse (package: your/package) [struct] // Response for getting first message
		{
		  Title string `json:"title,omitempty"` // Title of the message
		  Items []getFirstMessageItem `json:"items"` // List of message items
		}

		type getFirstMessageItem (package: your/package) [struct] // Individual message item
		{
		  Id int64 `json:"id,string,omitempty"` // Unique identifier
		  Preview string `json:"preview"` // Message preview
		  Text string `json:"text"` // Full message text
		}
	*/
}

type FunctionAnalyzer struct {
	visited      map[string]bool
	callStack    []string
	targetPkg    *PackageInfo
	targetFile   *ast.File
	debug        bool
	typeResolver *EnhancedTypeResolver
}

func NewFunctionAnalyzer(typeResolver *EnhancedTypeResolver, debug bool) *FunctionAnalyzer {
	return &FunctionAnalyzer{
		visited:      make(map[string]bool),
		callStack:    make([]string, 0),
		debug:        debug,
		typeResolver: typeResolver,
	}
}

func (fa *FunctionAnalyzer) FindFunction(funcRef string) (*ast.FuncDecl, error) {
	parts := strings.Split(funcRef, "/")
	if len(parts) == 0 {
		return nil, fmt.Errorf("invalid function reference")
	}

	funcName := parts[len(parts)-1]
	if fa.debug {
		fmt.Printf("🔍 Search function: %s\n", funcName)
	}

	for pkgName, pkg := range fa.typeResolver.ProjectInfo.Packages {
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				if fn, ok := decl.(*ast.FuncDecl); ok {
					curFuncName := fmt.Sprintf("%s.%s", pkg.Package.Name(), fn.Name.Name)
					if fn.Recv != nil {
						if t, ok := fn.Recv.List[0].Type.(*ast.StarExpr); ok {
							curFuncName = fmt.Sprintf("%s.%s.%s", pkg.Package.Name(), t.X.(*ast.Ident).Name, fn.Name.Name)
						}
					}
					if curFuncName == funcName {
						if fa.debug {
							fmt.Printf("✅ Found function %s in package %s, file %s\n", funcName, pkgName, file.Name)
						}
						fa.targetPkg = pkg
						fa.targetFile = file
						return fn, nil
					}
				}
			}
		}
	}
	return nil, fmt.Errorf("function %s not found", funcName)
}

func (fa *FunctionAnalyzer) FindFunctionsInTargetPackage() map[string]*ast.FuncDecl {
	functions := make(map[string]*ast.FuncDecl)

	if fa.targetPkg == nil {
		return functions
	}

	if fa.debug {
		fmt.Printf("🔍 Search function in: %s\n", fa.targetPkg.Package.Path())
	}

	if pkg, exists := fa.typeResolver.ProjectInfo.Packages[fa.targetPkg.Package.Path()]; exists {
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				if fn, ok := decl.(*ast.FuncDecl); ok {
					key := fmt.Sprintf("%s.%s", pkg.Package.Name(), fn.Name.Name)
					if fn.Recv != nil {
						if t, ok := fn.Recv.List[0].Type.(*ast.StarExpr); ok {
							key = fmt.Sprintf("%s.%s.%s", pkg.Package.Name(), t.X.(*ast.Ident).Name, fn.Name.Name)
						}
					}
					functions[key] = fn
					if fa.debug {
						fmt.Printf("📝 Function found: %s\n", fn.Name.Name)
					}
				}
			}
		}
	}
	return functions
}

func (fa *FunctionAnalyzer) FindJSONOperationsRecursive(fn *ast.FuncDecl, depth int) (unmarshalType, marshalType types.Type) {
	indent := strings.Repeat("  ", depth)
	if fa.debug {
		fmt.Printf("%s🔍 Analysing function: %s (depth %d)\n", indent, fn.Name.Name, depth)
	}

	if depth > 10 {
		if fa.debug {
			fmt.Printf("%s⚠️ Max recursion depth reached\n", indent)
		}
		return nil, nil
	}

	fa.callStack = append(fa.callStack, fn.Name.Name)
	defer func() {
		fa.callStack = fa.callStack[:len(fa.callStack)-1]
	}()

	// Find in the root func
	unmarshalType, marshalType = fa.findJSONInFunction(fn, depth)

	if unmarshalType != nil && marshalType != nil {
		return unmarshalType, marshalType
	}

	targetFunctions := fa.FindFunctionsInTargetPackage()

	ast.Inspect(fn, func(n ast.Node) bool {
		if call, ok := n.(*ast.CallExpr); ok {
			funcName := fa.extractFunctionName(call)
			if funcName != "" {
				if fa.debug {
					fmt.Printf("%s📞 Function call found: %s\n", indent, funcName)
				}

				if targetFn, exists := targetFunctions[funcName]; exists {
					if !fa.visited[funcName] {
						fa.visited[funcName] = true
						if fa.debug {
							fmt.Printf("%s🔄 Recursion analysis: %s\n", indent, funcName)
						}

						u, m := fa.FindJSONOperationsRecursive(targetFn, depth+1)
						if u != nil && unmarshalType == nil {
							unmarshalType = u
						}
						if m != nil && marshalType == nil {
							marshalType = m
						}
					}
				}
			}
		}
		return true
	})

	return unmarshalType, marshalType
}

func (fa *FunctionAnalyzer) extractFunctionName(call *ast.CallExpr) string {
	switch fun := call.Fun.(type) {
	case *ast.Ident:
		return fmt.Sprintf("%s.%s", fa.targetPkg.Package.Name(), fun.Name)
	case *ast.SelectorExpr:
		if funX, ok := fun.X.(*ast.Ident); ok {
			name := fmt.Sprintf("%s.%s", funX.Name, fun.Sel.Name)
			if funX.Obj == nil {
				return name
			}
			if funX.Obj.Kind != ast.Var {
				return name
			}
			if funXObjDecl, ok := funX.Obj.Decl.(*ast.Field); ok {
				if funXObjDeclType, ok := funXObjDecl.Type.(*ast.StarExpr); ok {
					if funXObjDeclTypeX, ok := funXObjDeclType.X.(*ast.Ident); ok {
						if funXObjDeclTypeX.Obj == nil {
							return name
						}
						if funXObjDeclTypeX.Obj.Kind != ast.Typ {
							return name
						}
						return fmt.Sprintf("%s.%s.%s", fa.targetPkg.Package.Name(), funXObjDeclTypeX.Name, fun.Sel.Name)
					}
					if funXObjDeclTypeX, ok := funXObjDeclType.X.(*ast.SelectorExpr); ok {
						if funXObjDeclTypeXX, ok := funXObjDeclTypeX.X.(*ast.Ident); ok {
							return fmt.Sprintf("%s.%s.%s", funXObjDeclTypeXX.Name, funXObjDeclTypeX.Sel.Name, fun.Sel.Name)
						}
					}
				}
			}
		}
	}
	return ""
}

func (fa *FunctionAnalyzer) findJSONInFunction(fn *ast.FuncDecl, depth int) (unmarshalType, marshalType types.Type) {
	indent := strings.Repeat("  ", depth)

	ast.Inspect(fn, func(n ast.Node) bool {
		if call, ok := n.(*ast.CallExpr); ok {
			if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
				if ident, ok := sel.X.(*ast.Ident); ok {
					if ident.Name == "json" && sel.Sel.Name == "Unmarshal" {
						if fa.debug {
							fmt.Printf("%s🎯 Found json.Unmarshal!\n", indent)
						}
						if len(call.Args) >= 2 {
							unmarshalType = fa.typeResolver.GetArgumentType(call.Args[1], fa.targetPkg.Package.Path())
							//unmarshalType = fa.extractTypeFromArg(call.Args[1], fn)
							if fa.debug {
								fmt.Printf("%s✅ Unmarshal Type: %s\n", indent, unmarshalType)
							}
						}
					}
					if ident.Name == "validator" && sel.Sel.Name == "ValidateTo" {
						if fa.debug {
							fmt.Printf("%s🎯 Found validator.ValidateTo!\n", indent)
						}
						if len(call.Args) >= 3 {
							unmarshalType = fa.typeResolver.GetArgumentType(call.Args[2], fa.targetPkg.Package.Path())
							//unmarshalType = fa.extractTypeFromArg(call.Args[1], fn)
							if fa.debug {
								fmt.Printf("%s✅ ValidateTo Type: %s\n", indent, unmarshalType)
							}
						}
					}

					if ident.Name == "json" && sel.Sel.Name == "Marshal" {
						if fa.debug {
							fmt.Printf("%s🎯 Found json.Marshal!\n", indent)
						}
						if len(call.Args) >= 1 {
							marshalType = fa.typeResolver.GetArgumentType(call.Args[0], fa.targetPkg.Package.Path())
							//marshalType = fa.extractTypeFromArg(call.Args[0], fn)
							if fa.debug {
								fmt.Printf("%s✅ Marshal Type: %s\n", indent, marshalType)
							}
						}
					}
					if sel.Sel.Name == "WriteJson" {
						if fa.debug {
							fmt.Printf("%s🎯 cw.WriteJson Found!\n", indent)
						}
						if len(call.Args) >= 2 {
							if arg, ok := call.Args[1].(*ast.SelectorExpr); ok && arg.Sel.Name == "StatusOK" {
								marshalType = fa.typeResolver.GetArgumentType(call.Args[0], fa.targetPkg.Package.Path())
								//marshalType = fa.extractTypeFromArg(call.Args[0], fn)
								if fa.debug {
									fmt.Printf("%s✅ WriteJson Type: %s\n", indent, marshalType)
								}
							}
						}
					}
				}
			}
		}
		return true
	})

	return unmarshalType, marshalType
}

func (fa *FunctionAnalyzer) AnalyzeFunction(funcRef string) error {
	if fa.debug {
		fmt.Printf("🚀 Start: %s\n", funcRef)
		fmt.Printf("===========================================\n\n")
	}

	fn, err := fa.FindFunction(funcRef)
	if err != nil {
		return err
	}

	// Сброс состояния для анализа
	fa.visited = make(map[string]bool)
	fa.callStack = make([]string, 0)

	unmarshalType, marshalType := fa.FindJSONOperationsRecursive(fn, 0)

	if fa.debug {
		fmt.Printf("\n📊 Results\n")
		fmt.Printf("===========================================\n\n")
	}

	if unmarshalType != nil {
		if fa.debug {
			fmt.Printf("1. ✅ json.Unmarshal Type: %s\n", unmarshalType)
		}

		structure := fa.typeResolver.GetArgumentTypeStructureWithComments(unmarshalType, fa.targetPkg.Package.Path())
		if structure != nil {
			if fa.debug {
				fmt.Printf("Full type structure: %s\n", unmarshalType.String())
			}
			fmt.Printf("Request:\n")
			fmt.Print(fa.typeResolver.PrintTypeStructure(structure, 0))
		}
		//if typeInfo := fa.FindTypeDefinition(unmarshalType); typeInfo != nil {
		//	fa.printTypeInfoWithNested(typeInfo, make(map[string]bool))
		//}
		fmt.Println()
	} else {
		fmt.Println("1. ❌ json.Unmarshal not found ", funcRef)
	}

	if marshalType != nil {
		if fa.debug {
			fmt.Printf("2. ✅ json.Marshal Type: %s\n", marshalType)
		}
		structure := fa.typeResolver.GetArgumentTypeStructureWithComments(marshalType, fa.targetPkg.Package.Path())
		if structure != nil {
			if fa.debug {
				fmt.Printf("Full type structure: %s\n", marshalType.String())
			}
			fmt.Printf("Response:\n")
			fmt.Print(fa.typeResolver.PrintTypeStructure(structure, 0))
		}
		//if typeInfo := fa.FindTypeDefinition(marshalType); typeInfo != nil {
		//	fa.printTypeInfoWithNested(typeInfo, make(map[string]bool))
		//}
		fmt.Println()
	} else {
		fmt.Println("2. ❌ json.Marshal not found ", funcRef)
	}

	return nil
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: go run main.go <method_name> <function_ref> <project_root>")
		fmt.Println("Example: go run main.go /my/method/ go.my.com/av/service/internal/handlers/chat.Create /path/to/project")
		os.Exit(1)
	}

	method := os.Args[1]
	funcRef := os.Args[2]
	projectRoot := os.Args[3]
	debug := false
	if len(os.Args) == 4 {
		debug = os.Args[3] == "true"
	}

	projectInfo, err := BuildProjectTypeInfo(projectRoot)
	if err != nil {
		log.Fatal(err)
	}

	typeResolver := CreateEnhancedTypeResolver(projectInfo)

	//fmt.Printf("Found %d packages:\n", len(projectInfo.Packages))
	//for pkgPath, pkgInfo := range projectInfo.Packages {
	//	fmt.Printf("- %s (files: %d)\n", pkgPath, len(pkgInfo.Files))
	//}

	analyzer := NewFunctionAnalyzer(typeResolver, debug)

	fmt.Printf("Method: %s\n", method)
	if err := analyzer.AnalyzeFunction(funcRef); err != nil {
		log.Fatalf("Error: %v", err)
	}
}
