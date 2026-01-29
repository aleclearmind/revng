//
// This file is distributed under the MIT License. See LICENSE.md for details.
//
#include <algorithm>
#include <cctype>
#include <clang/Format/Format.h>
#include <clang/Tooling/Core/Replacement.h>
#include <iostream>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <map>
#include <string>
#include <vector>

namespace {

// Represents a mapping from stripped text offset to original XML offset
struct OffsetMapping {
  size_t stripped_offset; // Offset in the stripped plain text
  size_t original_offset; // Corresponding offset in original XML
  int delta; // Cumulative characters removed up to this point
};

// Parse and expand basic XML entities
std::string expandEntity(const std::string &entity) {
  if (entity == "lt")
    return "<";
  if (entity == "gt")
    return ">";
  if (entity == "amp")
    return "&";
  if (entity == "quot")
    return "\"";
  if (entity == "apos")
    return "'";

  // Numeric entities
  if (!entity.empty() && entity[0] == '#') {
    if (entity.size() > 1 && entity[1] == 'x') {
      // Hexadecimal
      int code = std::stoi(entity.substr(2), nullptr, 16);
      return std::string(1, static_cast<char>(code));
    } else {
      // Decimal
      int code = std::stoi(entity.substr(1));
      return std::string(1, static_cast<char>(code));
    }
  }

  return "&" + entity + ";"; // Unknown entity, return as-is
}

// Strip XML tags and expand entities, building offset mapping
std::string stripXML(const std::string &xml,
                     std::vector<OffsetMapping> &mappings) {
  std::string result;
  size_t original_pos = 0;
  size_t stripped_pos = 0;
  int cumulative_removed = 0;

  while (original_pos < xml.size()) {
    // Record mapping at significant points
    mappings.push_back({ stripped_pos, original_pos, cumulative_removed });

    if (xml[original_pos] == '<') {
      // Find the end of the tag
      size_t tag_end = xml.find('>', original_pos);
      if (tag_end == std::string::npos) {
        // Malformed XML, treat '<' as regular character
        result += xml[original_pos];
        stripped_pos++;
        original_pos++;
        continue;
      }

      // Skip the entire tag
      size_t tag_length = tag_end - original_pos + 1;
      cumulative_removed += tag_length;
      original_pos = tag_end + 1;

    } else if (xml[original_pos] == '&') {
      // Find the end of the entity
      size_t entity_end = xml.find(';', original_pos);
      if (entity_end == std::string::npos) {
        // Malformed entity, treat '&' as regular character
        result += xml[original_pos];
        stripped_pos++;
        original_pos++;
        continue;
      }

      // Extract and expand the entity
      std::string entity = xml.substr(original_pos + 1,
                                      entity_end - original_pos - 1);
      std::string expanded = expandEntity(entity);

      result += expanded;
      stripped_pos += expanded.size();

      // Calculate how many characters we removed
      size_t entity_length = entity_end - original_pos + 1; // includes '&' and
                                                            // ';'
      cumulative_removed += (entity_length - expanded.size());

      original_pos = entity_end + 1;

    } else {
      // Regular character
      result += xml[original_pos];
      stripped_pos++;
      original_pos++;
    }
  }

  // Add final mapping
  mappings.push_back({ stripped_pos, original_pos, cumulative_removed });

  return result;
}

// Map stripped text offset back to original XML offset
size_t mapToOriginalOffset(size_t stripped_offset,
                           const std::vector<OffsetMapping> &mappings) {
  // Find the mapping entry at or before the stripped_offset
  auto it = std::upper_bound(mappings.begin(),
                             mappings.end(),
                             stripped_offset,
                             [](size_t offset, const OffsetMapping &mapping) {
                               return offset < mapping.stripped_offset;
                             });

  if (it != mappings.begin()) {
    --it;
  }

  // Calculate the original offset
  size_t offset_in_stripped = stripped_offset - it->stripped_offset;
  size_t original_offset = it->original_offset + offset_in_stripped;

  return original_offset;
}

// Apply formatting changes to original XML
std::string
applyFormattingToXML(const std::string &original_xml,
                     const std::string &stripped_text,
                     const clang::tooling::Replacements &replacements,
                     const std::vector<OffsetMapping> &mappings) {

  // Convert replacements from line/column to offsets in stripped text,
  // then map to original XML offsets
  struct Change {
    size_t original_offset;
    size_t length;
    std::string replacement_text;

    bool operator<(const Change &other) const {
      return original_offset < other.original_offset;
    }
  };

  std::vector<Change> changes;

  for (const auto &repl : replacements) {
    // repl.getOffset() returns the byte offset in stripped text
    size_t stripped_start = repl.getOffset();

    // Map to original XML offset
    size_t original_start = mapToOriginalOffset(stripped_start, mappings);

    // The length of text being replaced in stripped text
    size_t stripped_length = repl.getLength();

    changes.push_back({ original_start,
                        stripped_length,
                        repl.getReplacementText().str() });
  }

  // Sort changes by offset (in reverse to apply from end to start)
  std::sort(changes.begin(),
            changes.end(),
            [](const Change &a, const Change &b) {
              return a.original_offset > b.original_offset;
            });

  // Apply changes
  std::string result = original_xml;
  for (const auto &change : changes) {
    result.replace(change.original_offset,
                   change.length,
                   change.replacement_text);
  }

  return result;
}

// Main formatting function
std::string formatXMLContent(const std::string &xml_input) {
  // Step 1 & 2: Strip XML and build offset mappings
  std::vector<OffsetMapping> mappings;
  std::string stripped_text = stripXML(xml_input, mappings);

  std::cout << "Original XML (" << xml_input.size() << " bytes):\n"
            << xml_input << "\n\n";
  std::cout << "Stripped text (" << stripped_text.size() << " bytes):\n"
            << stripped_text << "\n\n";

  // Debug: Print mappings
  std::cout << "Offset mappings:\n";
  for (const auto &m : mappings) {
    std::cout << "  stripped[" << m.stripped_offset << "] -> original["
              << m.original_offset << "], delta=" << m.delta << "\n";
  }
  std::cout << "\n";

  // Step 3: Format the stripped text using libformat
  clang::format::FormatStyle style = clang::format::getLLVMStyle();
  style.ColumnLimit = 40; // Set a small column limit for testing

  // Create ranges to format (format entire file)
  std::vector<clang::tooling::Range> ranges;
  ranges.push_back(clang::tooling::Range(0, stripped_text.size()));

  // Calculate replacements
  auto replacements = clang::format::reformat(style,
                                              stripped_text,
                                              ranges,
                                              "input.cpp" // Pretend it's a C++
                                                          // file for formatting
  );

  std::cout << "Formatting replacements: " << replacements.size() << "\n";
  for (const auto &repl : replacements) {
    std::cout << "  At offset=" << repl.getOffset()
              << ", length=" << repl.getLength() << ", replace with: \""
              << repl.getReplacementText().str() << "\"\n";
  }
  std::cout << "\n";

  // Step 4 & 5: Apply formatting to original XML
  std::string result = applyFormattingToXML(xml_input,
                                            stripped_text,
                                            replacements,
                                            mappings);

  return result;
}
} // namespace

int main() {
  // Test case 1: Simple XML with entities
  std::string test1 = "<root>int x=5&lt;10;</root>";
  std::cout << "=== Test 1 ===\n";
  std::string result1 = formatXMLContent(test1);
  std::cout << "Result:\n" << result1 << "\n\n";

  // Test case 2: XML with multiple tags and entities
  std::string test2 = "<doc><para>int main(){return 0;}</para></doc>";
  std::cout << "=== Test 2 ===\n";
  std::string result2 = formatXMLContent(test2);
  std::cout << "Result:\n" << result2 << "\n\n";

  // Test case 3: XML with mixed content and entities
  std::string test3 = "<code>if(x&gt;0&amp;&amp;y&lt;100){z=x+y;}</code>";
  std::cout << "=== Test 3 ===\n";
  std::string result3 = formatXMLContent(test3);
  std::cout << "Result:\n" << result3 << "\n\n";

  // Test case 4: Nested tags
  std::string test4 = "<root><func>void foo(  int   a  ,  int b  "
                      "){}</func></root>";
  std::cout << "=== Test 4 ===\n";
  std::string result4 = formatXMLContent(test4);
  std::cout << "Result:\n" << result4 << "\n\n";

  // Test case 5: Code with quotes
  std::string test5 = "<snippet>char* s=&quot;hello&quot;;</snippet>";
  std::cout << "=== Test 5 ===\n";
  std::string result5 = formatXMLContent(test5);
  std::cout << "Result:\n" << result5 << "\n\n";

  return 0;
}
