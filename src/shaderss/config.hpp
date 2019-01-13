#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <wincodec.h> 

#include "common.hpp"

struct shader_info
{
  std::wstring  id                ;
  std::wstring  short_description ;
  std::wstring  author            ;
  std::wstring  license           ;
  std::wstring  modifications     ;
  std::wstring  notes             ;
  std::string   source            ;

  bool          requires_image    ;
};
using shader_infos = std::vector<shader_info> ;
shader_infos const & get__shader_infos ();

struct shader_configuration
{
  shader_info   shader_info     ;

  float         start_time      ;
  float         speed           ;

  std::wstring  image_path      ;
};
shader_configuration get__current_configuration ();
void set__current_configuration (shader_configuration const & configuration);

struct loaded_shader_configuration
{
  shader_configuration            shader_configuration    ;

  com_ptr<IWICFormatConverter>    image_converter         ;

  std::pair<UINT, UINT>           get__image_dimensions ();
  std::vector<BYTE>               get__image_bits       ();
};
loaded_shader_configuration load__configuration (shader_configuration const & configuration);
