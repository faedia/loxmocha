#!/bin/bash
set -euo pipefail

SPEC_DIR="spec"
OUTPUT_DIR="build"
TEMP_DIR="$OUTPUT_DIR/temp"
BASE_NAME="lox-mocha-specification"
PDF_NAME="${BASE_NAME}.pdf"
TEX_NAME="${BASE_NAME}.tex"
DEBUG=false  # Set to true to enable debug output

# Create directories
mkdir -p "$OUTPUT_DIR" "$TEMP_DIR"

SPEC_FILES=(
    "${SPEC_DIR}/introduction.md"
    "${SPEC_DIR}/lexical-grammar.md"
    "${SPEC_DIR}/declarations.md"
    "${SPEC_DIR}/expressions.md"
    "${SPEC_DIR}/types.md"
    "${SPEC_DIR}/patterns.md"
)

# Process markdown files to fix cross-references
echo "Removing filenames from links..."

printf '%s\n' "${SPEC_FILES[@]}" | while IFS= read -r file; do
    basename=$(basename "$file")
    echo "    Processing $basename"

    # Process the file
    # Convert [text](file.md#anchor) -> [text](#anchor)
    sed 's/\[\([^]]*\)\](\([^)]*\)\.md#\([^)]*\))/[\1](#\3)/g' "$file" > "$TEMP_DIR/$basename"
done

# Combine all files
echo "Combining files..."
printf '%s\n' "${SPEC_FILES[@]}" | while IFS= read -r file; do
    echo "    Adding $file"
    
    # Append file content to combined markdown file
    cat "$file" >> "$TEMP_DIR/combined.md"
    # Add newlines between files
    echo -e "\n\n" >> "$TEMP_DIR/combined.md"
done

# Generate LaTeX file
echo "Generating LaTeX file..."
pandoc --pdf-engine=xelatex --from markdown "$TEMP_DIR/combined.md" -o "$OUTPUT_DIR/$TEX_NAME" --template=${SPEC_DIR}/style/template.tex --top-level-division="chapter"
# Generate PDF
echo "Generating PDF..."
pandoc --pdf-engine=xelatex --from markdown "$TEMP_DIR/combined.md" -o "$OUTPUT_DIR/$PDF_NAME" --template=${SPEC_DIR}/style/template.tex --top-level-division="chapter"
echo "PDF generated: $OUTPUT_DIR/$PDF_NAME"
echo ""

if [ "$DEBUG" = true ]; then
    echo "Debug files kept in: $TEMP_DIR"
    echo "Check $TEMP_DIR/combined.md to verify cross-references"
else
    rm -rf "$TEMP_DIR"  # Clean up temporary files
    echo "Temporary files cleaned up."
fi
