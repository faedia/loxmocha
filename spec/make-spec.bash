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

# Process markdown files to fix cross-references
echo "Removing filenames from links..."
OIFS=$IFS
IFS=$'\n'
for file in $(find "$SPEC_DIR" -name "*.md" | sort -V); do
    basename=$(basename "$file")
    echo "    Processing $basename"

    # Process the file
    # Convert [text](file.md#anchor) -> [text](#anchor)
    sed 's/\[\([^]]*\)\](\([^)]*\)\.md#\([^)]*\))/[\1](#\3)/g' "$file" > "$TEMP_DIR/$basename"
done
IFS=$OIFS

# Combine all files
echo "Combining files..."
OIFS=$IFS
IFS=$'\n'
for file in $(find "$TEMP_DIR" -name "*.md" | sort -V); do
    echo "    Adding $file"
    
    # Append file content to combined markdown file
    cat "$file" >> "$TEMP_DIR/combined.md"
    # Add newlines between files
    echo -e "\n\n" >> "$TEMP_DIR/combined.md"
done
IFS=$OIFS

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